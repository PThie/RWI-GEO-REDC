testing_removed_variables <- function(
    dependency = NA
) {
    #' @title Testing removed variables
    #' 
    #' @description This function checks if variables removed due to missing data
    #' and compares the list of removed variables between the current and previous.
    #' The list(s) can be empty.
    #' 
    #' @param dependency Object from previous step to indicate dependency between
    #' functions/ steps
    #'  
    #' @return Vector with variables that have been removed and are different.
    #' Can be NULL, i.e. empty
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check if variables removed are consistent across versions
    # NOTE: not for v1 since there is no previous version

    if (config_globals()[["current_version"]] != "v1") {
        #--------------------------------------------------
        # define file path (to avoid repetition)

        file_location_current <- file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "info",
            "variables_deleted_due_to_missings.txt"
        )

        file_location_prev <- file.path(
            config_paths()[["output_path"]],
            config_globals()[["previous_version"]],
            "info",
            "variables_deleted_due_to_missings.txt"
        )

        #--------------------------------------------------
        # check in case nothing was removed (table is empty)

        if (file.info(file_location_current)$size == 0) {
            cli::cli_alert_success(
                cli::col_green(
                    "No variables were removed due to missing data in current version!"
                )
            )

            # define empty vector for comparison
            deleted_vars <- NULL
        } else {
            # read variables that were removed for current version
            deleted_vars <- utils::read.table(file_location_current) |>
                unlist() |>
                as.vector()
        }

        #--------------------------------------------------
        # do the same for previous version
        
        if (file.info(file_location_prev)$size == 0) {
            cli::cli_alert_success(
                cli::col_green(
                    "No variables were removed due to missing data in previous version!"
                )
            )

            # define empty vector for comparison
            deleted_vars_prev <- NULL
        } else {
            # read variables that were removed for previous version
            deleted_vars_prev <- utils::read.table(file_location_prev) |>
                unlist() |>
                as.vector()
        }

        #--------------------------------------------------
        # check if variables removed are consistent across versions

        if (length(deleted_vars) == 0 & length(deleted_vars_prev) == 0) {
            cli::cli_alert_success(
                cli::col_green(
                    "No variables were removed due to missing data in both versions!"
                )
            )
            # define empty vector for return
            difference <- NULL
        } else {
            # find the difference between both variable vectors
            difference <- setdiff(
                union(deleted_vars, deleted_vars_prev), # combines all unique elements
                intersect(deleted_vars, deleted_vars_prev) # finds the common elements
            )

            # only keep variables that have not been checked
            difference_not_checked <- difference[
                !difference %in% config_checked_variables_removed()
            ]

            targets::tar_assert_true(
                length(difference_not_checked) == 0,
                msg = glue::glue(
                    "!!! ERROR: ",
                    "The following variables have been removed and different from
                    previous cleaning steps: {glue::glue_collapse(difference, sep = ', ')}",
                    " (Error code: trv#1)"
                )
            )
        }
    } else {
        difference <- NULL
    }

    #--------------------------------------------------
    # return

    return(difference)
}