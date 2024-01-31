testing_consistent_variables <- function(current_delivery = NA) {
    #' @title Testing consistent variables
    #' 
    #' @description This file tests if the variables are consistent with the
    #' variables from previous years.
    #' 
    #' @return NULL
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
    # check if all variables are consistent with previous years

    # tar_assert_true(
    #     all(
    #         names(org_data) == config_variable_list()
    #     ),
    #     msg = "The variables are not consistent with previous years."
    # )

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

            fix_columns <- c(fix_columns, col)
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

    return(NULL)
}