reading_grids <- function(data_file_path = NA) {
    #' @title Read in grid data
    #' 
    #' @description This function reads in the grid data.
    #' 
    #' @param data_file_path Path to the grid data
    #' 
    #' @return Spatial dataframe
    #' @author Patrick Thiel

    #--------------------------------------------------
    # load data
    grids <- sf::st_read(
        data_file_path,
        quiet = TRUE
    ) |>
    # keep only grid ID and geometry columns
    dplyr::select(
        idm, geometry
    )

    # factor as character (turns the data set into characters)
    grids <- grids |>
        dplyr::mutate_if(
            is.factor,
            as.character
        )

    # transform 
    grids <- sf::st_transform(
        grids,
        crs = config_globals()[["utmcrs"]]
    )

    #--------------------------------------------------
    # return

    return(grids)
}