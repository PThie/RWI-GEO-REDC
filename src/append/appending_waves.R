appending_waves <- function(
    deliveries = NA,
    dependency = NA
) {
    #' @title Combining deliveries
    #' 
    #' @description This function combines all deliveries into one data set.
    #' 
    #' @param deliveries List with all deliveries
    #' @param dependency Object from previous step to indicate dependency between
    #' functions/ steps
    #' 
    #' @return Dataframe, combined data set
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # check dependency
    # NOTE: do something with dependency, so it actually matters that you have
    # passed it

    targets::tar_assert_nonempty(
        dependency,
        msg = glue::glue(
            "!!! ERROR: ",
            "The dependency  is empty.",
            " (Error code: aw#1)"
        )
    )

    #--------------------------------------------------
    # read all data

    data_storage <- list()
    for (del in deliveries) {
        if (del == 2306) {
            dta <- arrow::read_parquet(
                file.path(
                    config_paths()[["data_path"]],
                    "processed",
                    paste0("Lieferung_", del),
                    "fixed_bef",
                    "clean_data_fixed_bef.parquet"
                )
            )
        } else {
            dta <- arrow::read_parquet(
                file.path(
                    config_paths()[["data_path"]],
                    "processed",
                    paste0("Lieferung_", del),
                    "clean_data.parquet"
                )
            )
        }

        data_storage[[del]] <- dta
    }
    
    #--------------------------------------------------
    # identify new columns

    col_names_storage <- list()
    for (del in deliveries) {
        col_names <- names(data_storage[[del]])
        col_names_storage[[del]] <- col_names
    }

    # difference in columns of last delivery and previous delivery
    col_diff <- setdiff(
        col_names_storage[[length(col_names_storage)]],
        col_names_storage[[length(col_names_storage) - 1]]
    )

    for (col in col_diff) {
        # print new columns
        cli::cli_alert_info(
            cli::col_green(
                "New column in latest delivery: {col}. Add to config_new_variables() if needed."
            )
        )
    }

    # check that all new columns have been added
    current_delivery <- stringr::str_replace(
        config_globals()[["current_delivery"]],
        "Lieferung_",
        ""
    )
    
    targets::tar_assert_true(
        all(
            col_diff %in% config_new_variables()[[current_delivery]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "New columns in latest delivery are not added to config_new_variables().",
            " (Error code: aw#2)"
        )
    )

    #--------------------------------------------------
    # append all data

    dta_append <- data.table::rbindlist(data_storage, fill = TRUE)

    #--------------------------------------------------
    # return

    return(dta_append)
}