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
    # split housing data into consisting coordinates and without coordinates
    # because geo-referencing does not work with missings in coordinates

    org_data_wo_coords <- org_data_cleaned |>
        dplyr::filter(
            geox == -9
        )

    org_data_coords <- org_data_cleaned |>
        dplyr::filter(
            geox != -9
        )
    
    #----------------------------------------------
    # transform the coordinate system of the Immo data

    # this is the projection Immoscout uses
    projection_immo <- "+proj=lcc +lat_1=40 +lat_2=60 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

    # define as spatial data
    org_data_sf <- sf::st_as_sf(
        org_data_coords,
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

    # add zipcodes
    suppressWarnings(
        org_data_sf <- sf::st_join(
            org_data_sf,
            PLZ_2019,
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
                    "lon_utm"
                ),
                ~ as.numeric(.x)
            )
        ) |>
        dplyr::select(-idm)

    #----------------------------------------------
    # fill blid and zipcodes
    # NOTE: filling algorithm uses the given information first and the
    # information from the spatial join second

    org_data_prep <- org_data_prep |>
        dplyr::mutate(
            blid = as.character(blid),
            blid = dplyr::case_when(
                nchar(blid) == 1 ~ paste0("0", blid),
                TRUE ~ blid
            ),
            # recode proper NA for foalesce to work
            blid = dplyr::case_when(
                blid == "-9" ~ NA_character_,
                TRUE ~ blid
            ),
            blid_aux = substring(gid2019, 1, 2),
            # fill state ID (if missing) with ID obtained from spatial join
            blid = data.table::fcoalesce(blid, blid_aux),
            # do the same for zipcodes
            plz = dplyr::case_when(
                plz == "-9" ~ NA_character_,
                TRUE ~ plz
            ),
            plz = data.table::fcoalesce(plz, plz2019)
        ) |>
        dplyr::select(-c(blid_aux, plz2019)) |>
        # recode missings again to match missing definition of REDC/RED
        tidyr::replace_na(list(blid = "-9", plz = "-9"))

    #----------------------------------------------
    # fill municipality and district ID
    # NOTE: filling algorithm uses the given information first and the
    # information from the spatial join second

    org_data_prep <- org_data_prep |>
        dplyr::mutate(
            # generate helper variables for municipality ID using
            # the given Immo information
            gid2019_aux = as.character(gkz),
            gid2019_aux = dplyr::case_when(
                gid2019_aux == "-9" ~ NA_character_,
                TRUE ~ gid2019_aux
            ),
            gid2019_aux = dplyr::case_when(
                nchar(gid2019_aux) == 7 ~ paste0("0", gid2019_aux),
                TRUE ~ gid2019_aux
            ),
            # fill missing municipality ID with ID obtained from spatial join
            gid2019_aux = data.table::fcoalesce(gid2019_aux, gid2019),
            # do the same for district ID
            kid2019_aux = dplyr::case_when(
                !is.na(gid2019_aux) ~ substring(gid2019_aux, 1, 5),
                TRUE ~ NA_character_
            ),
            kid2019_aux = data.table::fcoalesce(kid2019_aux, kid2019)
        ) |>
        dplyr::select(-c(gid2019, kid2019)) |>
        dplyr::rename(
            gid2019 = gid2019_aux,
            kid2019 = kid2019_aux
        )

    #----------------------------------------------
    # add geo IDs to the data set with no coordinates

    org_data_wo_coords <- org_data_wo_coords |>
        dplyr::mutate(
            lon_gps = -9,
            lat_gps = -9,
            lon_utm = -9,
            lat_utm = -9,
            ergg_1km = "-9",
            # use the given municipality information to add municipality and
            # district ID inline with the data set with coordinates
            gid2019 = as.character(gkz),
            gid2019 = dplyr::case_when(
                nchar(gid2019) == 7 ~ paste0("0", gid2019),
                TRUE ~ gid2019
            ),
            kid2019 = dplyr::case_when(
                !is.na(gid2019) ~ substring(gid2019, 1, 5),
                TRUE ~ NA_character_
            ),
            # set state ID as character to match the data set with coordinates
            blid = as.character(blid),
            blid = dplyr::case_when(
                nchar(blid) == 1 ~ paste0("0", blid),
                TRUE ~ blid
            )
        )

    # merge municipality names
    org_data_wo_coords <- merge(
        org_data_wo_coords,
        GEM_2019 |>
            sf::st_drop_geometry(),
        by = "gid2019",
        all.x = TRUE
    )

    # merge district names
    org_data_wo_coords <- merge(
        org_data_wo_coords,
        KRS_2019 |>
            sf::st_drop_geometry(),
        by = "kid2019",
        all.x = TRUE
    )

    #----------------------------------------------
    # combine both data sets again (with and without coordinates)

    org_data_prep <- dplyr::bind_rows(
        org_data_prep,
        org_data_wo_coords
    )
    
    #----------------------------------------------
    # drop given municipality information
    # because not needed anymore (incorparated in the other spatial IDs)

    org_data_prep <- org_data_prep |>
        dplyr::select(-gkz)

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
    # return output

    return(org_data_prep)
}