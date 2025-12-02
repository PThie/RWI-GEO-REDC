sampling_tagesspiegel_berlin_data <- function(
    housing_data = NA
) {
    #' @title Sampling Tagesspiegel Berlin Data
    #' 
    #' @description This function samples and processes housing data specific
    #' to Berlin.
    #' 
    #' @param housing_data Data frame containing housing data.
    #' 
    #' @return Spatial data frame with Berlin-specific housing data.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read Berlin Stadtteile shapefile

    berlin_stadtteile <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Stadtteile\\Berlin/RBS_OD_ORT_1412.shp"
        ),
            quiet = TRUE
    ) |>
    sf::st_transform(crs = 4326) |>
    dplyr::rename(
        stadtteil_id = ORT,
        stadtteil_name = Ortsteilna,
        bezirk_id = Beznr,
        bezirk_name = Bezname
    )

    #--------------------------------------------------
    # subset housing data to Berlin

    berlin_data <- housing_data |>
        dplyr::filter(kid2019 == "11000")

    #--------------------------------------------------
    # set as spatial feature

    berlin_data_sf <- sf::st_as_sf(
        berlin_data,
        coords = c("lon_gps", "lat_gps"),
        crs = 4326,
        remove = FALSE
    )

    #--------------------------------------------------
    # add nearest Stadtteil

    nearest <- sf::st_nearest_feature(
        berlin_data_sf,
        berlin_stadtteile
    )

    berlin_data_sf <- berlin_data_sf |>
        dplyr::mutate(
            stadtteil_id = berlin_stadtteile$stadtteil_id[nearest],
            stadtteil_name = berlin_stadtteile$stadtteil_name[nearest],
            bezirk_id = berlin_stadtteile$bezirk_id[nearest],
            bezirk_name = berlin_stadtteile$bezirk_name[nearest]
        )

    #--------------------------------------------------
    
    return(berlin_data_sf)
}