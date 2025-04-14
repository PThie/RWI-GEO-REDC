adding_spatial_units <- function(
    housing_data = NA,
    spatial_data = NA
) {
    #' @title Adding spatial units to the housing data
    #' 
    #' @description This function adds spatial units to the housing data (only
    #' for properties with coordinates).
    #' 
    #' @param housing_data Prepared housing data.
    #' @param spatial_data Spatial dataframe for different spatial units.
    #' 
    #' @return Dataframe with spatial units.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # make point locations spatial data based on UTM

    housing_data_sf <- sf::st_as_sf(
        housing_data,
        coords = c("lon_utm", "lat_utm"),
        crs = config_globals()[["utmcrs"]],
        remove = FALSE
    )

    #--------------------------------------------------
    # merge housing data with spatial data

    suppressWarnings(
        housing_data_sf <- sf::st_join(
            housing_data_sf,
            spatial_data,
            left = TRUE,
            largest = TRUE
        )
    )

    #--------------------------------------------------
    # drop geometry

    housing_data_prep <- sf::st_drop_geometry(housing_data_sf)

    #--------------------------------------------------
    # return 

    return(housing_data_prep)
}