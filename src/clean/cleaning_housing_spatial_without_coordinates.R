cleaning_housing_spatial_without_coordinates <- function(
    housing_data = NA,
    spatial_data_district = NA,
    spatial_data_municipality = NA
) {
    #' @title Cleaning housing data without coordinates
    #' 
    #' @description This function cleans the housing data without coordinates
    #' and adds the same columns as for the housing data with coordinates.
    #' 
    #' @param housing_data DataFrame with housing data without coordinates
    #' @param spatial_data_district Spatial data of the districts
    #' @param spatial_data_municipality Spatial data of the municipalities
    #' 
    #' @return DataFrame with cleaned housing data (without coordinates)
    #' @author Patrick Thiel

    #--------------------------------------------------
    # add geo IDs to the data set with no coordinates

    housing_data_wo_coords <- housing_data |>
        dplyr::select(-blid) |>
        dplyr::mutate(
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

    #--------------------------------------------------
    # merge municipality names
    
    housing_data_wo_coords <- merge(
        housing_data_wo_coords,
        spatial_data_municipality |>
            sf::st_drop_geometry(),
        by = "gid2019",
        all.x = TRUE
    )

    #--------------------------------------------------
    # merge district names
    
    housing_data_wo_coords <- merge(
        housing_data_wo_coords,
        spatial_data_district |>
            sf::st_drop_geometry(),
        by = "kid2019",
        all.x = TRUE
    )

    #--------------------------------------------------
    # return

}