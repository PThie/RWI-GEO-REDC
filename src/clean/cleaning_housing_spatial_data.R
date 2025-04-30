cleaning_housing_spatial_data <- function(
    housing_data = NA
) {
    #' @title Cleaning housing spatial data
    #' 
    #' @description This function cleans the housing data with coordinates
    #' after the other spatial data has been added.
    #' 
    #' @param housing_data Prepared housing data.
    #' 
    #' @return Dataframe with cleaned housing data.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # replace missing coordinates (not in Germany)

    housing_data_prep <- housing_data |>
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

    #--------------------------------------------------
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

    #--------------------------------------------------
    # return

    return(housing_data_prep)
}