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

    avg_microm_beginning <- microm_data_prep |>
        dplyr::filter(year %in% c(2005, 2009)) |>
        dplyr::group_by(ergg_1km) |>
        dplyr::summarise(
            num_business = mean(num_business, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # create dataframe containing years 2007 to now

    microm_beginning <- rbind(
        microm_data_prep |>
            dplyr::filter(year != 2005) |>
            dplyr::mutate(
                num_business = as.numeric(num_business)
            ),
        avg_microm_beginning |>
            dplyr::mutate(
                year = 2007
            ),
        avg_microm_beginning |>
            dplyr::mutate(
                year = 2008
            )
    )

    #--------------------------------------------------
    # create average values for years not yet in microm data

    avg_microm_end <- microm_data_prep |>
        dplyr::filter(
            year %in% c(
                config_globals()[["microm_max_year"]] - 1,
                config_globals()[["microm_max_year"]]
            )
        ) |>
        dplyr::group_by(ergg_1km) |>
        dplyr::summarise(
            num_business = mean(num_business, na.rm = TRUE)
        ) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # duplicate data for end periods

    if (config_globals()[["max_year"]] > config_globals()[["microm_max_year"]]) {
        # determine years for duplication
        end_years <- seq(
            config_globals()[["microm_max_year"]] + 1,
            config_globals()[["max_year"]],
            by = 1
        )

        # duplicate data
        end_years_data_list <- list()
        for (year in end_years) {
            end_years_data_list[[year]] <- avg_microm_end |>
                dplyr::mutate(
                    year = year
                )
        }

        # combine all
        end_years_data <- data.table::rbindlist(end_years_data_list)
    }

    #--------------------------------------------------
    # merge all datasets

    # determine if housing data and microm have different time horizons
    difference_years <- config_globals()[["max_year"]] -
        config_globals()[["microm_max_year"]]

    # merge all datasets
    if (difference_years > 0) {
        microm_complete <- rbind(
            microm_beginning,
            end_years_data
        )
    } else {
        microm_complete <- microm_beginning
    }

    #--------------------------------------------------
    # add dummy for censoring
    # NOTE: if the censoring threshold is undercut, the ergg_1km value is masked

    microm_complete <- microm_complete |>
        dplyr::mutate(
            censor_grid_id = dplyr::case_when(
                num_business < config_globals()[["censoring_threshold_businesses"]] ~ 1,
                TRUE ~ 0
            ),
            # make year integer to match housing data
            year = as.integer(year)
        ) |>
        dplyr::select(-num_business)

    #--------------------------------------------------
    # return

    return(microm_complete)
}