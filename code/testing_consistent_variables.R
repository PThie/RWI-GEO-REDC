testing_consistent_variables <- function(org_data = NA, current_delivery = NA) {
    #' @title Testing consistent variables
    #' 
    #' @param org_data Raw original data
    #' @param current_delivery The current delivery.
    #' 
    #' @description This file tests if the variables are consistent with the
    #' variables from previous years.
    #' 
    #' @return Dataframe with original data where missing columns have been added
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read original data

    org_data <- data.table::fread(
        file.path(
            config_paths()[["data_path"]],
            "original",
            current_delivery,
            "commercial_data_all.csv"
        )
    )

    #----------------------------------------------
    # print differences

    fix_columns <- c()

    for (col in config_variable_list()) {
        if (!(col %in% names(org_data))) {
            cli::cli_alert_danger(
                cli::col_red(
                    "The following variable is not consistent with previous years: ",
                    col
                )
            )
            # store the variable that needs to be fixed
            fix_columns <- c(fix_columns, col)

            # update org data
            org_data[[col]] <- NA
        }
    }

    tar_assert_true(
        all(
            # NOTE: Add here all variables that have been fixed
            fix_columns == c("Energieeffizienz_Klasse")
        ),
        msg = "All variable differences have been already fixed in the following."
    )

    #----------------------------------------------
    # return

    return(org_data)
}