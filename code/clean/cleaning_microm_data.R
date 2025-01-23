cleaning_microm_data <- function(
    microm_data = NA
) {
    #' @title Cleaning microm data
    #' 
    #' @description This function cleans the microm data needed for anonymization
    #' in the SUF housing data.
    #' 
    #' @param microm_data Dataframe with microm data
    #' 
    #' @return Dataframe with cleaned microm data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # keep only relevant columns

    microm_data_prep <- microm_data |>
        dplyr::select(
            ergg_1km = r1_id,
            year,
            num_business = r1_mba_a_gewerbe
        )

    #--------------------------------------------------
    # create average value for years 2005 and 2009 to fill years 2007 to 2008

    avg_microm <- microm_data_prep |>
        dplyr::filter(year %in% c(2005, 2009)) |>
        dplyr::group_by(ergg_1km) |>
        dplyr::summarise(
            num_business = mean(num_business, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # create dataframe containing years 2007 to now

    microm_complete <- rbind(
        microm_data_prep |>
            dplyr::filter(year != 2005) |>
            dplyr::mutate(
                num_business = as.numeric(num_business)
            ),
        avg_microm |>
            dplyr::mutate(
                year = 2007
            ),
        avg_microm |>
            dplyr::mutate(
                year = 2008
            )
    )

    #--------------------------------------------------
    # add dummy for censoring
    # NOTE: if the censoring threshold is undercut, the ergg_1km value is masked

    microm_complete <- microm_complete |>
        dplyr::mutate(
            censor_grid_id = dplyr::case_when(
                num_business < config_globals()[["censoring_threshold_businesses"]] ~ 1,
                TRUE ~ 0
            )
        ) |>
        dplyr::select(-num_business)

    #--------------------------------------------------
    # return

    return(microm_complete)
}