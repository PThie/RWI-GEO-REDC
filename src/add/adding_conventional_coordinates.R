adding_conventional_coordinates <- function(
    housing_data = NA
) {
    #' @title Adding conventional coordinates to the housing data
    #' 
    #' @description This function adds conventional coordinates (GPS and UTM) to
    #' the housing data.
    #' 
    #' @param housing_data Prepared housing data.
    #' 
    #' @return Dataframe with conventional coordinates.
    #' @author Patrick Thiel

    #----------------------------------------------
    # split housing data into consisting coordinates and without coordinates
    # because geo-referencing does not work with missings in coordinates

    housing_data_wo_coords <- housing_data |>
        dplyr::filter(
            geox == helpers_missing_values()[["other"]]
        )

    housing_data_coords <- housing_data |>
        dplyr::filter(
            geox != helpers_missing_values()[["other"]]
        )

    #----------------------------------------------
    # transform the coordinate system of the Immo data

    # define as spatial data
    housing_data_sf <- sf::st_as_sf(
        housing_data_coords,
        coords = c("geox", "geoy"),
        crs = config_globals()[["projection_immo"]],
        remove = FALSE
    )

    #----------------------------------------------
    # transformation to GPS projection

    # transform to GPS
    lonlat_gps <- sf::st_transform(
        housing_data_sf,
        crs = config_globals()[["gpscrs"]]
    )

    # get coordinates
    coords_gps <- sf::st_coordinates(lonlat_gps) |>
        as.data.frame() |>
        dplyr::rename(
            lon_gps = X,
            lat_gps = Y
        )

    # merge to original data
    housing_data_sf <- cbind(
        housing_data_sf,
        coords_gps
    )

    #----------------------------------------------
    # transformation to UTM projection

    # transform to UTM
    lonlat_utm <- sf::st_transform(
        housing_data_sf,
        crs = config_globals()[["utmcrs"]]
    )

    # get coordinates
    coords_utm <- sf::st_coordinates(lonlat_utm) |>
        as.data.frame() |>
        dplyr::rename(
            lon_utm = X,
            lat_utm = Y
        )

    # merge to original data
    housing_data_sf <- cbind(
        housing_data_sf,
        coords_utm
    )

    # remove geometry from data
    housing_data_prep <- sf::st_drop_geometry(housing_data_sf)

    #----------------------------------------------
    # add geo IDs to the data set with no coordinates

    housing_data_wo_coords <- housing_data_wo_coords |>
        dplyr::mutate(
            lon_gps = helpers_missing_values()[["other"]],
            lat_gps = helpers_missing_values()[["other"]],
            lon_utm = helpers_missing_values()[["other"]],
            lat_utm = helpers_missing_values()[["other"]]
        )

    #----------------------------------------------
    # combine both data sets in a list (with and without coordinates)

    housing_data_list <- list(
        "housing_data_coords" = housing_data_prep,
        "housing_data_wo_coords" = housing_data_wo_coords
    )

    #--------------------------------------------------
    # return

    return(housing_data_list)
}