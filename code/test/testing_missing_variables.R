testing_missing_variables <- function(
    housing_data = NA
) {
    #' @title Testing missing variables
    #' 
    #' @description This function checks the number of missings and censored values
    #' for each variable and reports them for documentation. It also removes all
    #' variables that are 100% missing.
    #' 
    #' @param housing_data Dataframe with housing data
    #' 
    #' @return Dataframe with housing data witout variables that are 100% missing
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # calculate the number of missings per variable

    # function to calculate missings by data type
    calculating_missings <- function(data_type, missing_type) {
        missings <- housing_data |>
            dplyr::summarise(
                dplyr::across(
                    .cols = dplyr::where(data_type),
                    ~ sum(.x %in% missing_type)
                )
            ) |>
            tidyr::pivot_longer(
                cols = everything(),
                names_to = "variable",
                values_to = "missings"
            )

        return(missings)
    }

    # apply function to calculate missings and combine both types
    missings <- rbind(
        calculating_missings(is.integer, missing_type = c(-9)),
        calculating_missings(is.character, missing_type = c("-9"))
    ) |>
        dplyr::arrange(dplyr::desc(missings)) |>
        # calculate percentage of missings
        dplyr::mutate(
            missings_perc = round(
                missings / nrow(housing_data) * 100,
                digits = 4
            )
        )

    # calculate missings including censored values
    all_missing_specification <- as.numeric(unlist(helpers_missing_values()))

    missing_incl_censored <- rbind(
        calculating_missings(is.integer, missing_type = all_missing_specification),
        calculating_missings(is.character, missing_type = all_missing_specification |> as.character())
    ) |>
        dplyr::arrange(dplyr::desc(missings)) |>
        dplyr::rename(missings_incl_censored = missings) |>
        # calculate percentage of missings
        dplyr::mutate(
            missings_incl_censored_perc = round(
                missings_incl_censored / nrow(housing_data) * 100,
                digits = 4
            )
        )

    #--------------------------------------------------
    # merge both missings tables (without and with censored values)

    all_missings <- merge(
        missings,
        missing_incl_censored,
        by = "variable",
        all = TRUE
    ) |>
    dplyr::arrange(dplyr::desc(missings_incl_censored))

    # export for documentation
    openxlsx::write.xlsx(
        all_missings,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "number_of_missings.xlsx"
        ),
        row.names = FALSE
    )

    #--------------------------------------------------
    # extract all variables that have 100% missing
    # I am not including censored values because technically the information is
    # there

    all_missing_variable <- all_missings |>
        dplyr::filter(missings_perc == 100) |>
        dplyr::select(variable) |>
        dplyr::pull()

    for (var in all_missing_variable) {
        # NOTE: Exclude bef variables from this because they belong to a group
        # of variables
        if (!grepl("bef", var)) {
            housing_data[[var]] <- NULL
        

            cli::cli_alert_info(
                cli::col_yellow(
                    glue::glue(
                        "Variable {var} has been removed due to 100% missings."
                    )
                )
            )
        }
    }

    # export list of variables to check with future waves
    utils::write.table(
        all_missing_variable,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "variables_deleted_due_to_missings.txt"
        ),
        row.names = FALSE,
        col.names = FALSE
    )

    #--------------------------------------------------
    # export data

    fst::write.fst(
        housing_data,
        file.path(
            config_paths()[["data_path"]],
            "processed",
            config_globals()[["current_delivery"]],
            "clean_data.fst"
        )
    )

    #--------------------------------------------------
    # return

    return(housing_data)
}