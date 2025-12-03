calculating_obs_tagesspiegel <- function(
    housing_data = NA
) {
    #' @title Calculating Observations Tagesspiegel Berlin
    #' 
    #' @description This function calculates the number of observations
    #' grouped by Stadtteil and housing type from the Tagesspiegel Berlin
    #' housing data.
    #' 
    #' @param housing_data Spatial data frame containing Berlin housing data.
    #' 
    #' @return Data frame with counts of observations by Stadtteil and
    #' housing type.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # drop geometry for easier handling

    housing_data_df <- housing_data |>
        sf::st_drop_geometry()

    #--------------------------------------------------
    # calculate observations by Stadtteil and housing type

    housing_data_df <- housing_data_df |>
        dplyr::mutate(
            housing_type = dplyr::case_when(
                # store, Sales area, Department store
                category_business %in% c(2, 4, 24) ~ "einzelhandel",
                # restaurant, cafe, Bar service and Lounge
                category_business %in% c(10, 13, 18) ~ "gastronomie",
                TRUE ~ "andere"
            )
        )

    obs_stadtteil <- housing_data_df |>
        dplyr::group_by(stadtteil_name, housing_type) |>
        dplyr::summarise(
            n_obs = dplyr::n()
        ) |>
        dplyr::ungroup()

    #--------------------------------------------------
    # export as csv

    data.table::fwrite(
        obs_stadtteil,
        file = file.path(
            config_paths()[["data_path"]],
            "special_requests",
            "Tagesspiegel_2025",
            "obs_tagesspiegel_berlin.csv"
        ),
        sep = ";",
        encoding = "UTF-8"
    )

    #--------------------------------------------------
    # return

    return(obs_stadtteil)
}