georeferencing <- function(org_data_cleaned = org_data_cleaned) {
    #' @title Geocoding the RED observations
    #' 
    #' @description This function transforms the giving reference system into the
    #' standard reference systems of GPS and UTM.
    #' 
    #' @param org_data_cleaned Prepared original data
    #' 
    #' @return DataFrame
    #' @author Patrick Thiel
    #TODO: add state ID if not already in the code
    # TODO: Remove GKZ
    # TODO: Use geocoordinate to find all geographical units and if no geo coordinate
    # is found, use Immo info (like GKZ, PLZ, Kreis) to fill in blanks (drop Immo variables afterwards)
    org_data_cleaned = read.fst(
        file.path(
            config_paths()[["data_path"]],
            "processed",
            "Lieferung_2306",
            "clean_data.fst"
        )
    )

    names(org_data_cleaned)
    
    #----------------------------------------------
    # function for reading data

    read_geo_data <- function(
        disgem = c("Kreis", "Gemeinde", "PLZ"),
        year = 2019,
        ags_name = NA,
        gen_name = NA
    ) {
        #' @title Read in district shape data
        #' 
        #' @param disgem Indicator for district or municipality data
        #' @param year Year of district shape data
        #' @param ags_name How AGS should be renamed after reading
        #' @param gen_name How GEN should be renamed after reading
        
        #----------------------------------------------
        # abbreviation for district or municipality
        if (disgem == "Kreis") {
            filename <- "VG250_KRS"
            region <- disgem
        } else if (disgem == "Gemeinde") {
            filename <- "VG250_GEM"
            region <- disgem
        } else {
            filename <- "PLZ"
            region <- "Postleitzahl"
        }

        # read data
        dta <- sf::st_read(
            file.path(
                config_paths()[["gebiete_path"]],
                region,
                year,
                paste0(filename, ".shp")
            ),
            quiet = TRUE
        )

        if (disgem != "PLZ") {
            dta <- dta |>
                # keep only AGS and geometry columns
                dplyr::select(
                    AGS, GEN, geometry
                ) |>
                # rename AGS
                dplyr::rename(
                    !!rlang::sym(ags_name) := AGS,
                    !!rlang::sym(gen_name) := GEN
                )
        } else {
            dta <- dta |>
                dplyr::rename(
                    !!rlang::sym(ags_name) := PLZ
                )
        }

        # convert factors
        dta |>
            dplyr::mutate_if(is.factor, as.character) -> dta

        # transform to UTM
        dta <- sf::st_transform(
            dta,
            crs = config_globals()[["utmcrs"]]
        )

        #----------------------------------------------
        # return output

        return(dta)
    }

    #----------------------------------------------
    # district data

    # apply function
    KRS_2019 <- read_geo_data(
        disgem = "Kreis",
        year = 2019,
        ags_name = "kid2019",
        gen_name = "kname2019"
    )

    #----------------------------------------------
    # muncipality data

    GEM_2019 <- read_geo_data(
        disgem = "Gemeinde",
        year = 2019,
        ags_name = "gid2019",
        gen_name = "gname2019"
    )

    #----------------------------------------------
    # zipcode data

    PLZ_2019 <- read_geo_data(
        disgem = "PLZ",
        year = 2019,
        ags_name = "plz2019"
    )

    #----------------------------------------------
    # grid data

    # load data
    grids <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Raster",
            "grids_BRD.shp"
        ),
        quiet = TRUE
    ) |>
    # keep only grid ID and geometry columns
    dplyr::select(
        idm, geometry
    )

    # factor as character (turns the data set into characters)
    grids <- grids |>
        dplyr::mutate_if(
            is.factor,
            as.character
        )

    # transform 
    grids <- sf::st_transform(
        grids,
        crs = config_globals()[["utmcrs"]]
    )

    #----------------------------------------------
    # transform the coordinate system of the Immo data

    # this is the projection Immoscout uses
    projection_immo <- "+proj=lcc +lat_1=40 +lat_2=60 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

    # define as spatial data
    org_data_sf <- sf::st_as_sf(
        org_data_cleaned,
        coords = c("geox", "geoy"),
        crs = projection_immo
    )

    #----------------------------------------------
    # transformation to GPS projection

    # transform to GPS
    lonlat_gps <- sf::st_transform(
        org_data_sf,
        crs = config_globals()[["gpscrs"]]
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
    org_data_sf <- cbind(
        org_data_sf,
        coords_gps
    )

    #----------------------------------------------
    # transformation to UTM projection

    # transform to UTM
    lonlat_utm <- sf::st_transform(
        org_data_sf,
        crs = config_globals()[["utmcrs"]]
    )

    # get coordinates
    coords_utm <- sf::st_coordinates(lonlat_utm) |>
        as.data.frame() |>
        dplyr::rename(
            lon_utm = X,
            lat_utm = Y
        )

    # merge to original data
    org_data_sf <- cbind(
        org_data_sf,
        coords_utm
    )

    # remove geometry from data
    org_data_sf <- sf::st_drop_geometry(org_data_sf)

    #----------------------------------------------
    # Merge other geo data

    # make point locations spatial data based on UTM
    org_data_sf <- sf::st_as_sf(
        org_data_sf,
        coords = c("lon_utm", "lat_utm"),
        crs = config_globals()[["utmcrs"]],
        remove = FALSE
    )

    # add districts
    suppressWarnings(
        org_data_sf <- sf::st_join(
            org_data_sf,
            KRS_2019,
            left = TRUE,
            largest = TRUE
        )
    )

    # add municipalities
    suppressWarnings(
        org_data_sf <- sf::st_join(
            org_data_sf,
            GEM_2019,
            left = TRUE,
            largest = TRUE
        )
    )

    # add grid
    suppressWarnings(
        org_data_sf <- sf::st_join(
            org_data_sf,
            grids,
            left = TRUE,
            largest = TRUE
        )
    )

    # drop geometry
    org_data_prep <- sf::st_drop_geometry(org_data_sf)

    # replace missing coordinates (not in Germany)
    org_data_prep <- org_data_prep |>
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    "lat_gps", "lon_gps",
                    "lat_utm", "lon_utm"
                ),
                ~ dplyr::case_when(
                    is.na(gid2019) ~ -9,
                    TRUE ~ .x
                )
            ),
            idm = tidyr::replace_na(idm, "-9")
        )

    # set types and names
    org_data_prep <- org_data_prep |>
        dplyr::mutate(
            ergg_1km = as.character(idm),
            dplyr::across(
                .cols = c(
                    "lat_gps", "lon_gps", "lat_utm",
                    "lon_utm", "gid2019", "kid2019"
                ),
                ~ as.numeric(.x)
            )
        ) |>
        dplyr::select(-idm)

    #----------------------------------------------
    # fill blid and zipcodes

    #----------------------------------------------
    # export

    fst::write.fst(
        org_data_prep,
        file.path(
            config_paths()[["data_path"]],
            "processed",
            current_delivery,
            "clean_data_georeferenced.fst"
        )
    )

    #----------------------------------------------

    return(org_data_prep)

}