appending_housing_with_without_coordinates <- function(
    housing_data_with_coordinates = NA,
    housing_data_without_coordinates = NA
) {
    #' @title Appending housing data with and without coordinates
    #' 
    #' @description This function appends the housing data with and without
    #' coordinates.
    #' 
    #' @param housing_data_with_coordinates DataFrame with coordinates
    #' @param housing_data_without_coordinates DataFrame without coordinates
    #' 
    #' @return DataFrame with complete housing data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check that both datasets are built the same

    targets::tar_assert_true(
        ncol(housing_data_with_coordinates) == ncol(housing_data_without_coordinates),
        msg = glue::glue(
            "The number of columns in both datasets is not the same. Please check!",
            " (Error code: ahwwc#1)"
        )
    )

    targets::tar_assert_true(
        all(
            names(housing_data_with_coordinates) %in%
            names(housing_data_without_coordinates)
        ),
        msg = glue::glue(
            "The column names of both datasets are not the same. Please check!",
            " (Error code: ahwwc#2)"
        )
    )

    #----------------------------------------------
    # combine both data sets again (with and without coordinates)

    housing_data_prep <- dplyr::bind_rows(
        housing_data_with_coordinates,
        housing_data_without_coordinates
    )

    #--------------------------------------------------
    # drop given municipality information
    # because not needed anymore (incorparated in the other spatial IDs)

    housing_data_prep <- housing_data_prep |>
        dplyr::select(-gkz)

    #--------------------------------------------------
    # return

    return(housing_data_prep)
}