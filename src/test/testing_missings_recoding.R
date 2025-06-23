testing_missings_recoding <- function(
    suf_data = NA,
    file_format = NA
) {
    #' @title Test for missing values that have not been recoded
    #' 
    #' @description This function tests if there are still missing values in the
    #' SUF data that have not been recoded. If there are, it raises an error.
    #'
    #' @param suf_data Dataframe with the SUF data
    #' @param file_format File format of the SUF data (character)
    #' 
    #' @return Dataframe with variables that still have missing values
    #' @author Patrick Thiel

    #--------------------------------------------------
    # test for missing values that have not been recoded

    # calculate number of missings per variable
    still_missings <- sapply(
        suf_data,
        function(x) {
            sum(is.na(x))
        }
    ) |>
    as.data.frame() |>
    dplyr::rename(
        num_missings = 1
    ) |>
    dplyr::filter(num_missings > 0)

    # reset column names
    still_missings$vars <- rownames(still_missings)
    rownames(still_missings) <- NULL

    # check if there are still missings
    targets::tar_assert_true(
        nrow(still_missings) == 0,
        msg = glue::glue(
            "!!! WARNING: ",
            "There are still missing values in the data that are not recoded. ",
            "Check the following variables for {file_format}: ",
            "{paste(still_missings$vars, collapse = ', ')}",
            " (Error code: tmr#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(still_missings)
}