cleaning_SUF_data <- function(
    housing_data = NA
) {
    #' @title Cleaning SUF data
    #' 
    #' @description This function cleans the SUF data.
    #' 
    #' @param housing_data Dataframe with SUF data
    #' 
    #' @return Dataframe with cleaned SUF data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check that variables that are supposed to be removed are in housing data

    for (var in helpers_deleted_variables_SUF()) {
        targets::tar_assert_true(
            var %in% names(housing_data),
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable {var} is not in the data set.",
                " (Error code: csd#1)"
            )
        )
    }

    #--------------------------------------------------
    # Remove variables that should not be in the SUF file

    housing_data <- housing_data |>
        dplyr::select(-helpers_deleted_variables_SUF())

    #--------------------------------------------------
    # return

    return(housing_data)
}