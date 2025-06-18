cleaning_missings_new_variables <- function(housing_data = NA) {
    #' @title Cleaning missing values in new variables
    #' 
    #' @description This function cleans the missing values in new variables.
    #' 
    #' @param housing_data Dataframe with housing data
    #' 
    #' @return Dataframe with cleaned missing values
    #' @author Patrick Thiel

    #--------------------------------------------------
    # replace missing values with appropriate encoding

    del <- stringr::str_extract(
        config_globals()[["current_delivery"]],
        "[0-9]+"
    )

    # extract matching delivery number
    del_num <- config_globals()[["deliveries_matching"]][[del]] 

    for (var in config_new_variables()[[del]]) {
        # extract 
        # check that new variables are indeed always missing
        delivery_data <- housing_data |>
            dplyr::filter(
                redc_delivery == del_num
            )

        targets::tar_assert_true(
            all(
                unique(delivery_data[[var]]) %in% c(NA)
            ),
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable {var} is not missing in delivery.",
                " (Error code: cmnv#1)"
            )
        )

        # replace with appropriate missing value
        if (typeof(housing_data[[var]]) == "character") {
        housing_data <- housing_data |>
            dplyr::mutate(
                !!rlang::sym(var) := dplyr::case_when(
                    is.na(!!rlang::sym(var)) & redc_delivery == del_num ~ as.character(
                        helpers_missing_values()[["not_used_anymore"]]
                    ),
                    TRUE ~ !!rlang::sym(var)
                )
            )
        } else {
            housing_data <- housing_data |>
                dplyr::mutate(
                    !!rlang::sym(var) := dplyr::case_when(
                        is.na(!!rlang::sym(var)) & redc_delivery == del_num ~ helpers_missing_values()[["not_used_anymore"]],
                        TRUE ~ !!rlang::sym(var)
                    )
                )
        }

        # check that the transformation worked
        num_missings <- length(which(is.na(delivery_data[[var]])))
        num_replaced <- length(which(
            housing_data |>
                dplyr::filter(redc_delivery == del_num) |>
                dplyr::select(var) == helpers_missing_values()[["not_used_anymore"]]
        ))

        targets::tar_assert_true(
            num_missings == num_replaced,
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable transformation for {var} did not work.",
                " (Error code: cmnv#2)"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(housing_data)
}
