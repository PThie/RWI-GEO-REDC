testing_consistent_variables <- function(
    housing_data = NA,
    column_infos_benchmark = NA
) {
    #' @title Testing consistent variables
    #' 
    #' @description This function compares the variable names and types of the
    #' current version with that of the very first delivery. If there are
    #' differences, the function will return a list with the variables that need
    #' to be fixed.
    #' 
    #' @param housing_data Dataframe with original raw data.
    #' @param column_infos_benchmark Dataframe with column names and types of
    #' the very first delivery.
    #' 
    #' @return List with variables that need to be fixed.
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # get column names and types of current delivery

    coltypes <- sapply(housing_data, class) |>
        unlist() |>
        as.data.frame() |>
        dplyr::rename(columns_types_current = 1)

    coltypes$columns <- rownames(coltypes)
    rownames(coltypes) <- NULL

    coltypes <- coltypes |>
        dplyr::relocate(columns) |>
        dplyr::filter(
            columns != "Einstelldatum2"
        ) |>
        dplyr::mutate(
            columns = dplyr::case_when(
                columns == "Einstelldatum1" ~ "Einstelldatum",
                TRUE ~ columns
            )
        )
    
    #----------------------------------------------
    # test for differences

    fix_columns <- c()
    fix_types <- c()

    for (col in coltypes$columns) {
        # check if column in in original set of variables (first delivery)
        if (!(col %in% column_infos_benchmark$columns)) {
            cli::cli_alert_danger(
                cli::col_red(
                    "The following variable is not consistent with original data: ",
                    col
                )
            )
            # store the variable that needs to be fixed
            fix_columns <- c(fix_columns, col)
        }

        # check if column type matches
        coltype_current <- coltypes |>
            dplyr::filter(columns == col) |>
            dplyr::select(columns_types_current) |>
            dplyr::pull()

        coltype_benchmark <- column_infos_benchmark |>
            dplyr::filter(columns == col) |>
            dplyr::select(columns_types) |>
            dplyr::pull()

        if (coltype_current != coltype_benchmark) {
            cli::cli_alert_danger(
                cli::col_red(
                    "The following variable has a different type than in the original data: ",
                    col
                )
            )
            # store the variable that needs to be fixed
            fix_types <- c(fix_types, col)
        }
    }

    # combine both vectors for export
    fix <- list(
        "columns" = fix_columns,
        "types" = fix_types
    )

    #--------------------------------------------------
    # check that stops the pipeline

    # test for consistent variables names
    targets::tar_assert_true(
        all(
            fix_columns %in% config_fixed_variables()[["fixed_names_vars"]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all variable names match the original data! ",
            "(Error code: tcv#1)"
        )
    )

    # test for consistent variable types
    targets::tar_assert_true(
        all(
            fix_columns %in% config_fixed_variables()[["fixed_types_vars"]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all variable types match the original data! ",
            "(Error code: tcv#2)"
        )
    )

    #----------------------------------------------
    # return

    return(fix)
}