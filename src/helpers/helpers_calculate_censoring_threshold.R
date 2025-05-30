helpers_calculate_censoring_threshold <- function(
    housing_data = NA,
    variable_of_interest = NA,
    variable_eng = NA,
    threshold = NA,
    threshold_type = NA
) {
    #' @title Calculate censoring threshold
    #' 
    #' @description This function calculates the censoring threshold for a
    #' given variable based on quantiles and the given thresholds.
    #' 
    #' @param housing_data Dataframe containing the housing data.
    #' @param variable_of_interest Character string with the variable of interest.
    #' @param variable_eng Character string with the variable of interest in English.
    #' @param threshold Numeric vector with the thresholds.
    #' @param threshold_type Character string with the type of threshold.
    #' 
    #' @return List with the calculated value and a dataframe listing all inputed
    #' information (needed for reporting).
    #' @author Patrick Thiel

    #--------------------------------------------------
    # make sure input column is numeric (otherwise quantile will not work)

    targets::tar_assert_true(
        is.numeric(housing_data[[variable_of_interest]]),
        msg = glue::glue(
            "!!! ERROR: ",
            "Variable '{variable_of_interest}' is not numeric.",
            " (Error code: hcct#1)"
        )
    )

    #--------------------------------------------------
    # calculate percentile value

    value <- as.numeric(
        quantile(
            x = housing_data |>
                dplyr::filter(!!rlang::sym(variable_of_interest) > 0) |>
                dplyr::select(variable_of_interest),
            probs = threshold,
            na.rm = TRUE
        )
    )

    #--------------------------------------------------
    # create dataframe for export later

    df <- data.frame(
        variable = variable_eng,
        threshold = threshold,
        threshold_type = threshold_type,
        value = round(value, digits = 2)
    )

    #--------------------------------------------------
    # combine everything

    out <- list(
        "value" = value,
        "dataframe" = df
    )

    #--------------------------------------------------
    # return

    return(out)
}