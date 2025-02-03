testing_SUF_anonymization <- function(
    suf_data = NA,
    microm_data = NA
) {
    #' @title Test SUF anonymization
    #' 
    #' @description This function tests whether the SUF data has been anonymized
    #' by the microm data correctly.
    #' 
    #' @param suf_data Dataframe with SUF data
    #' @param microm_data Dataframe with microm data
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # merge housing data and microm (GRID) data

    merged_data <- merge(
        suf_data,
        microm_data,
        by.x = c("ergg_1km", "ejahr"),
        by.y = c("ergg_1km", "year"),
        all.x = TRUE
    )

    # add id as combination of year and grid-ID
    merged_data <- merged_data |>
        dplyr::mutate(
            grid_year_id = paste0(ergg_1km, "_", ejahr)
        )

    # do the same for microm data
    microm_data <- microm_data |>
        dplyr::mutate(
            grid_year_id = paste0(ergg_1km, "_", year)
        )

    #--------------------------------------------------
    # filter observations that should be censored

    # test whether censor_grid_id only contains 0 or NA
    # NOTE: NA arise because the data has been anonymized and therefore, the
    # merge above leads to NAs
    targets::tar_assert_true(
        all(c(0, NA) %in% unique(merged_data$censor_grid_id)),
        msg = glue::glue(
            "!!! WARNING: ",
            "The censor_grid_id contains values other than 0 or NA.",
            " (Error code: tsa#1)"
        )
    )

    # identify unique grids that are not in microm data (due to no population)
    # drop also missing values (due to censoring earlier)
    grids_not_in_microm <- unique(merged_data$grid_year_id)[
        !(unique(merged_data$grid_year_id) %in% unique(microm_data$grid_year_id)) &
        (substring(unique(merged_data$grid_year_id), 1, 1) != "-")
    ]

    # filter observations that should be censored
    censored_data <- merged_data |>
        dplyr::filter(
            !(grid_year_id %in% grids_not_in_microm)
        ) |>
        dplyr::filter(is.na(censor_grid_id))

    # get grids that should be censored
    grids_censored <- unique(censored_data$ergg_1km)

    # test if all remaining grids are censored
    targets::tar_assert_true(
        all(grids_censored %in% c("-9", "-7")),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all remaining grids are censored.",
            " (Error code: tsa#2)"
        )
    )

    #--------------------------------------------------
    # return

    return(NULL)
}