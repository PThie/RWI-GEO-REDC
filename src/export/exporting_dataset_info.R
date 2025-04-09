exporting_dataset_info <- function(
    housing_data = NA
) {
    #' @title Exporting dataset information
    #' 
    #' @description This function collects information about the dataset and
    #' exports it to a text file for reporting.
    #' 
    #' @param housing_data Dataframe with cleaned housing data.
    #' 
    #' @return Dataframe with information about the dataset.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # collect information about the dataset

    num_rows <- nrow(housing_data)
    num_cols <- ncol(housing_data)
    col_names <- colnames(housing_data)

    # time horizon
    housing_data <- housing_data |>
        dplyr::mutate(
            year_mon = paste0("ejahr", - , "emonat")
        )

    start_date <- min(housing_data$year_mon, na.rm = TRUE)
    end_date <- max(housing_data$year_mon, na.rm = TRUE)

    # combine all information
    infos <- as.data.frame(
        cbind(
            Variables = c(
                "Number of rows:",
                "Number of columns:",
                "Column names:",
                "Start date:",
                "End date:"
            ),
            Values = c(
                num_rows,
                num_cols,
                paste(col_names, collapse = ", "),
                start_date,
                end_date
            )
        )
    )

    # define export path
    directory <- file.path(
        config_paths()[["output_path"]],
        config_globals()[["current_version"]],
        "info",
        "dataset_info.txt"
    )

    # export dates
    gdata::write.fwf(
        infos,
        directory,
        append = TRUE,
        rownames = FALSE,
        colnames = FALSE,
        quote = FALSE
    )

    #--------------------------------------------------
    # return

    return(infos)
}