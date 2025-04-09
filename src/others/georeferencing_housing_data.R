georeferencing_housing_data <- function(
    housing_data = NA,
    spatial_data_grids = NA,
    spatial_data_zip_code = NA,
    spatial_data_municipality = NA,
    spatial_data_district = NA,
    spatial_data_lmr = NA
) {
    #' @title Geocoding the RED observations
    #' 
    #' @description This function transforms the giving reference system into the
    #' standard reference systems of GPS and UTM.
    #' 
    #' @param housing_data Prepared housing data
    #' @param spatial_data_grids Spatial data of the grids
    #' @param spatial_data_zip_code Spatial data of the zip codes
    #' @param spatial_data_municipality Spatial data of the municipalities
    #' @param spatial_data_district Spatial data of the districts
    #' @param spatial_data_lmr Spatial data of the LMRs
    #' 
    #' @return DataFrame
    #' @author Patrick Thiel

    #----------------------------------------------
    # split housing data into consisting coordinates and without coordinates
    # because geo-referencing does not work with missings in coordinates

    housing_data_wo_coords <- housing_data |>
        dplyr::filter(
            geox == helpers_missing_values()[["other"]]
        )

    housing_data_coords <- housing_data |>
        dplyr::filter(
            geox != helpers_missing_values()[["other"]]
        )

    #----------------------------------------------
    # transform the coordinate system of the Immo data

    # define as spatial data
    housing_data_sf <- sf::st_as_sf(
        housing_data_coords,
        coords = c("geox", "geoy"),
        crs = config_globals()[["projection_immo"]],
        remove = FALSE
    )

    #----------------------------------------------
    # transformation to GPS projection

    # transform to GPS
    lonlat_gps <- sf::st_transform(
        housing_data_sf,
        crs = config_globals()[["gpscrs"]]
    )

    # get coordinates
    coords_gps <- sf::st_coordinates(lonlat_gps) |>
        as.data.frame() |>
        dplyr::rename(
            lon_gps = X,
            lat_gps = Y
        )

    # merge to original data
    housing_data_sf <- cbind(
        housing_data_sf,
        coords_gps
    )

    #----------------------------------------------
    # transformation to UTM projection

    # transform to UTM
    lonlat_utm <- sf::st_transform(
        housing_data_sf,
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
    housing_data_sf <- cbind(
        housing_data_sf,
        coords_utm
    )

    # remove geometry from data
    housing_data_sf <- sf::st_drop_geometry(housing_data_sf)

    #----------------------------------------------
    # Merge other geo data

    # make point locations spatial data based on UTM
    housing_data_sf <- sf::st_as_sf(
        housing_data_sf,
        coords = c("lon_utm", "lat_utm"),
        crs = config_globals()[["utmcrs"]],
        remove = FALSE
    )

    # add districts
    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data_district,
            left = TRUE,
            largest = TRUE
        )
    )

    # add municipalities
    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data_municipality,
            left = TRUE,
            largest = TRUE
        )
    )

    # add zipcodes
    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data_zip_code,
            left = TRUE,
            largest = TRUE
        )
    )

    # add grid
    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data_grids,
            left = TRUE,
            largest = TRUE
        )
    )

    # add LMR
    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data_lmr,
            left = TRUE,
            largest = TRUE
        )
    )

    # drop geometry
    housing_data_prep <- sf::st_drop_geometry(housing_data_sf)

    # replace missing coordinates (not in Germany)
    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    "lat_gps", "lon_gps",
                    "lat_utm", "lon_utm"
                ),
                ~ dplyr::case_when(
                    is.na(gid2019) ~ helpers_missing_values()[["other"]],
                    TRUE ~ .x
                )
            ),
            idm = tidyr::replace_na(idm, as.character(helpers_missing_values()[["other"]]))
        )

    # set types and names
    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            ergg_1km = as.character(idm),
            dplyr::across(
                .cols = c(
                    "lat_gps", "lon_gps",
                    "lat_utm", "lon_utm"
                ),
                ~ as.numeric(.x)
            )
        ) |>
        dplyr::select(-idm)

    #----------------------------------------------
    # define state ID and fill zipcodes
    # NOTE: filling algorithm uses the given information first and the
    # information from the spatial join second

    housing_data_prep <- housing_data_prep |>
        # drop original state ID    
        dplyr::select(-blid) |>
        dplyr::mutate(
            # extract state ID from district ID
            blid = substring(kid2019, 1, 2),
            # replace missings in zipcode with "calculated" information
            plz = dplyr::case_when(
                plz == as.character(helpers_missing_values()[["other"]]) ~ NA_character_,
                TRUE ~ plz
            ),
            plz = data.table::fcoalesce(plz, plz2019)
        ) |>
        dplyr::select(-plz2019) |>
        # recode missings again to match missing definition of REDC/RED
        tidyr::replace_na(list(
            blid = as.character(helpers_missing_values()[["other"]]),
            plz = as.character(helpers_missing_values()[["other"]])
        ))

    #----------------------------------------------
    # fill municipality and district ID
    # NOTE: filling algorithm uses the given information first and the
    # information from the spatial join second

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            # generate helper variables for municipality ID using
            # the given Immo information
            gid2019_aux = as.character(gkz),
            gid2019_aux = dplyr::case_when(
                gid2019_aux == as.character(helpers_missing_values()[["other"]]) ~ NA_character_,
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

    housing_data_wo_coords <- housing_data_wo_coords |>
        dplyr::select(-blid) |>
        dplyr::mutate(
            lon_gps = helpers_missing_values()[["other"]],
            lat_gps = helpers_missing_values()[["other"]],
            lon_utm = helpers_missing_values()[["other"]],
            lat_utm = helpers_missing_values()[["other"]],
            ergg_1km = as.character(helpers_missing_values()[["other"]]),
            lmr2018 = as.character(helpers_missing_values()[["other"]]),
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
            blid = substring(kid2019, 1, 2)
        ) |>
        tidyr::replace_na(list(
            blid = as.character(helpers_missing_values()[["other"]])
        ))

    # merge municipality names
    housing_data_wo_coords <- merge(
        housing_data_wo_coords,
        spatial_data_municipality |>
            sf::st_drop_geometry(),
        by = "gid2019",
        all.x = TRUE
    )

    # merge district names
    housing_data_wo_coords <- merge(
        housing_data_wo_coords,
        spatial_data_district |>
            sf::st_drop_geometry(),
        by = "kid2019",
        all.x = TRUE
    )

    #----------------------------------------------
    # combine both data sets again (with and without coordinates)

    housing_data_prep <- dplyr::bind_rows(
        housing_data_prep,
        housing_data_wo_coords
    )
    
    #----------------------------------------------
    # drop given municipality information
    # because not needed anymore (incorparated in the other spatial IDs)

    housing_data_prep <- housing_data_prep |>
        dplyr::select(-gkz)

    #----------------------------------------------
    # return output

    return(housing_data_prep)
}