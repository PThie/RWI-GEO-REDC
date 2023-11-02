georeferencing <- function(org_data = org_data) {
    
}

#----------------------------------------------
# description

# reads in the newest RED data delivery and transforms the Immo-reference
# system to GPS-reference system

#----------------------------------------------
# libraries

library(data.table)
library(rgeos)
library(proj4)
library(haven)
library(sf)
library(dplyr)
library(rlang)

#----------------------------------------------
# paths

# directory path
path_dir <-  "M:/_FDZ/RWI-GEO/RWI-GEO-RED/"

# path to geo info
path_geb <- "M:/_FDZ/interne Daten/Gebietseinheit/"

# path to original RED data
path_red <- "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/"

#----------------------------------------------
# directory and delivery

# set directory path
setwd(path_dir)

# define the current delivery
currdel <- 2306 # !!!! TO DO NEW DELIVERY !!!!

#----------------------------------------------
# district data

# function for reading data
read_geo_data <- function(disgem = c("Kreis", "Gemeinde"), year = c(2015, 2019), ags_name) {
    #' Read in district shape data
    #' 
    #' @param disgem Indicator for district or municipality data
    #' @param year Year of district shape data
    #' @param ags_name How AGS should be renamed after reading
    
    #----------------------------------------------
    # abbreviation for district or municipality
    if(disgem == "Kreis") {
        abb <- "KRS"
    } else {
        abb <- "GEM"
    }

    # read data
    dta <- sf::st_read(
        file.path(
            path_geb,
            disgem, year, paste0("VG250_", abb, ".shp")
        ),
        quiet = TRUE
    ) |>
    # keep only AGS and geometry columns
    select(
        AGS, geometry
    )

    # convert factors
    dta |>
        mutate_if(is.factor, as.character) -> dta

    # rename AGS
    dta <- dta |>
        rename(
            !!rlang::sym(ags_name) := AGS
        )

    # transform to UTM
    dta <- st_transform(
        dta, crs = 25832
    )

    return(dta)
}

# apply function
KRS_2015 <- read_geo_data(
    disgem = "Kreis",
    year = 2015,
    ags_name = "KID2015"
)

KRS_2019 <- read_geo_data(
    disgem = "Kreis",
    year = 2019,
    ags_name = "KID2019"
)

#----------------------------------------------
# muncipality data

GEM_2015 <- read_geo_data(
    disgem = "Gemeinde",
    year = 2015,
    ags_name = "GID2015"
)

GEM_2019 <- read_geo_data(
    disgem = "Gemeinde",
    year = 2019,
    ags_name = "GID2019"
)

#----------------------------------------------
# grid data

# load data
G_1km <- sf::st_read(
    file.path(
        path_geb,
        "Raster/grids_BRD.shp"
    ),
    quiet = TRUE
) |>
# keep only grid ID and geometry columns
dplyr::select(
    idm, geometry
)

# factor as character (turns the data set into characters)
G_1km |>
    dplyr::mutate_if(
        is.factor,
        as.character
    ) -> G_1km

# rename
names(G_1km)[names(G_1km) == "idm"] <- "idm_grid"

# transform 
G_1km <- sf::st_transform(
    G_1km,
    crs = 25832
)

#----------------------------------------------
# immo data

geo_fehl <- data.table::data.table(
    haven::read_dta(
        file = file.path(
            path_red,
            "locations",
            paste0(currdel,"-needsGeocoding.dta")
        ),
        encoding = NULL,
        col_select = NULL,
        skip = 0,
        n_max = Inf,
        .name_repair = "unique" 
    )
)

#----------------------------------------------
# transform the coordinate system of the Immo data

# store the coordinates in a separate data frame
coordinates <- geo_fehl |>
    dplyr::select(geox, geoy)

# this is the projection Immoscout uses
projection_immo <- "+proj=lcc +lat_1=40 +lat_2=60 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# define as spatial data
coordinates_sf <- sf::st_as_sf(
    coordinates,
    coords = c("geox", "geoy"),
    crs = projection_immo
)

#----------------------------------------------
# transformation to GPS projection

# transform to GPS
lonlat_gps <- sf::st_transform(
    coordinates_sf,
    crs = 4326
)

# get coordinates
coords_gps <- sf::st_coordinates(lonlat_gps) |>
    as.data.frame() |>
    dplyr::rename(
        lon_gps = X,
        lat_gps = Y
    ) |>
    # round coordinates
    # because through transformation it looks like that we have great precision
    # in the geo data but there is actually an inaccuracy (ca. 10m I think)
    dplyr::mutate(
        lon_gps = round(
            lon_gps,
            digits = 4
        ),
        lat_gps = round(
            lat_gps,
            digits = 4
        )
    )

# merge to original data
added_coords <- cbind(
    geo_fehl, coords_gps
)

#----------------------------------------------
# transformation to UTM projection

# transform to UTM
lonlat_utm <- sf::st_transform(
    coordinates_sf,
    crs = 25832
)

# get coordinates
coords_utm <- sf::st_coordinates(lonlat_utm) |>
    as.data.frame() |>
    dplyr::rename(
        lon_utm = X,
        lat_utm = Y
    )

# merge to original data
added_coords <- cbind(
    added_coords, coords_utm
)

#----------------------------------------------
# Merge other geo data

# make point locations spatial data
added_coords_sf <- sf::st_as_sf(
    added_coords,
    coords = c("lon_utm", "lat_utm"),
    crs = 25832,
    remove = FALSE
)

# add districts
dis2015 <- sf::st_join(
    added_coords_sf,
    KRS_2015,
    left = TRUE,
    largest = TRUE
)

dis2019 <- sf::st_join(
    dis2015,
    KRS_2019,
    left = TRUE,
    largest = TRUE
)

# add municipalities
muc2015 <- sf::st_join(
    dis2019,
    GEM_2015,
    left = TRUE,
    largest = TRUE
)

muc2019 <- sf::st_join(
    muc2015,
    GEM_2019,
    left = TRUE,
    largest = TRUE
)

# add grid
grid_merged <- sf::st_join(
    muc2019,
    G_1km,
    left = TRUE,
    largest = TRUE
)

# remove the wrongly determined coordinates
cleaned <- grid_merged |>
    dplyr::filter(!is.na(idm_grid))

# drop geometry
# rename
# change type
cleaned <- cleaned |>
    sf::st_drop_geometry() |>
    dplyr::rename(
        ergG_1km = idm_grid
    ) |>
    dplyr::mutate(
        lon_gps = as.character(lon_gps),
        lat_gps = as.character(lat_gps),
        lon_utm = as.character(lon_utm),
        lat_utm = as.character(lat_utm)
    )

#----------------------------------------------
# export

haven::write_dta(
    cleaned,
    file.path(
        path_red, 
        "locations",
        paste0(currdel, "-Geocoded.dta")
    ),
    version = 15
)