testing_summary_stats <- function(
    housing_data = NA,
    variable_labels = NA
) {
    #' @title Calculate summary statistics by delivery
    #' 
    #' @description This function calculates summary statistics for each numeric
    #' column in the dataset, by delivery.
    #' 
    #' @param housing_data Dataframe containing the housing data.
    #' @param variable_labels Dataframe containing the variable labels and their
    #' types.
    #' 
    #' @return List containing the summary statistics for each numeric column
    #' @author Patrick Thiel

    #--------------------------------------------------
    # identify numeric columns

    numeric_cols <- variable_labels |>
        dplyr::filter(
            `variable type` == "numeric"
        ) |>
        dplyr::pull(
            variable
        )
    
    #--------------------------------------------------
    # function to calculate summary statistics by delivery

    calculate_summary_stats <- function(
        housing_data = NA,
        variable_of_interest = NA
    ) {
        stats <- housing_data |>
            dplyr::mutate(
                !!variable_of_interest := dplyr::case_when(
                    !!rlang::sym(variable_of_interest) < 0 ~ NA,
                    TRUE ~ !!rlang::sym(variable_of_interest)
                )
            ) |>
            dplyr::group_by(redc_delivery) |>
            dplyr::summarise(
                min = min(!!rlang::sym(variable_of_interest), na.rm = TRUE),
                mean = mean(!!rlang::sym(variable_of_interest), na.rm = TRUE),
                median = median(!!rlang::sym(variable_of_interest), na.rm = TRUE),
                max = max(!!rlang::sym(variable_of_interest), na.rm = TRUE)
            ) |>
            dplyr::ungroup() |>
            tidyr::pivot_longer(
                cols = -redc_delivery,
                names_to = "stat",
                values_to = "value"
            ) |>
            as.data.frame()
        
        return(stats)
    }

    #--------------------------------------------------
    # create empty text file for output
    # because otherwise every time the pipeline is run the file is appended
    # and potentially old output is stored

    directory <- file.path(
        config_paths()[["output_path"]],
        config_globals()[["current_version"]],
        "stats",
        "summary_stats_by_delivery.txt"
    )

    write(
        "",
        directory,
        append = FALSE
    )

    #--------------------------------------------------
    # calculate summary statistics for each numeric column

    stats_list <- list()
    for (col in numeric_cols) {
        stats <- calculate_summary_stats(
            housing_data = housing_data,
            variable_of_interest = col
        )

        # define header
        header <- glue::glue(
            "
            #--------------------------------------------------
            Summary statistics for {col}
            #--------------------------------------------------
            "
        )

        # export header
        write(
            header,
            directory,
            append = TRUE
        )

        # export stats
        gdata::write.fwf(
            stats,
            directory,
            append = TRUE,
            rownames = FALSE,
            width = 10
        )

        # store stats in list
        stats_list[[col]] <- stats
    }

    #--------------------------------------------------
    # return

    return(stats_list)
}