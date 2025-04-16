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

    if (config_globals()[["current_delivery"]] == "Lieferung_2312") {
        for (var in config_new_variables()[["2312"]]) {
            # check that new variables are indeed always missing in the first delivery
            first_delivery <- housing_data |>
                dplyr::filter(redc_delivery == 1)

            targets::tar_assert_true(
                all(
                    unique(first_delivery[[var]]) %in% c(NA)
                ),
                msg = glue::glue(
                    "!!! WARNING: ",
                    "Variable {var} is not missing in first delivery.",
                    " (Error code: cmnv#1)"
                )
            )

            # replace with appropriate missing value
            housing_data <- housing_data |>
                dplyr::mutate(
                    !!rlang::sym(var) := dplyr::case_when(
                        is.na(!!rlang::sym(var)) & redc_delivery == 1 ~ helpers_missing_values()[["not_used_anymore"]],
                        TRUE ~ !!rlang::sym(var)
                    )
                )

            # check that the transformation worked
            num_missings <- length(which(is.na(first_delivery[[var]])))
            num_replaced <- length(which(
                housing_data |>
                    dplyr::filter(redc_delivery == 1) |>
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
    } 

    #--------------------------------------------------
    # return

    return(housing_data)
}
