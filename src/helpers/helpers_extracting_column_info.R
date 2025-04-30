helpers_extracting_column_info <- function(
    housing_data = NA
) {
    #' @title Extract column information from a dataset
    #' 
    #' @description This function extracts column information from a dataset.
    #' 
    #' @param auto_data Dataframe with housing data.
    #' 
    #' @return Dataframe with column names and types.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # get column names and types

    coltypes <- sapply(housing_data, class) |>
        unlist() |>
        as.data.frame() |>
        dplyr::rename(columns_types = 1)

    coltypes$columns <- rownames(coltypes)
    rownames(coltypes) <- NULL

    #--------------------------------------------------
    # handle date column

    coltypes <- coltypes |>
        dplyr::relocate(columns) |>
        # handle that Einstelldatum has two types
        dplyr::filter(
            columns != "Einstelldatum2"
        ) |>
        dplyr::mutate(
            columns = dplyr::case_when(
                columns == "Einstelldatum1" ~ "Einstelldatum",
                TRUE ~ columns
            )
        )

    #--------------------------------------------------
    # return

    return(coltypes)
}
