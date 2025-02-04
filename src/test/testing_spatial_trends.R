testing_spatial_trends <- function(
    housing_data = NA,
    districts = NA
) {
    #' @title Test spatial trends
    #' 
    #' @description This function tests the spatial trends of the housing market
    #' by plotting the evolution of prices and rents over time.
    #' 
    #' @param housing_data Dataframe with the housing data
    #' @param districts Spatial dataframe with the districts
    #' 
    #' @return NULL, direct output to file
    #' @author Patrick Thiel

    #--------------------------------------------------
    # set proper NA

    housing_data_prep <- housing_data |>
        dplyr::mutate(
            dplyr::across(
                .cols = c("listing_price", "usable_area", "rent_per_sqm"),
                ~ dplyr::case_when(
                    .x < 0 ~ NA,
                    TRUE ~ .x
                )
            )
        )
    
    #--------------------------------------------------
    # summarize prices and rents by district and year

    housing_data_prices <- housing_data_prep |>
        dplyr::mutate(
            listing_price_sqm = listing_price / usable_area
        ) |>
        dplyr::group_by(ejahr, kid2019) |>
        dplyr::summarize(
            listing_price_sqm = mean(listing_price_sqm, na.rm = TRUE),
            rent_per_sqm = mean(rent_per_sqm, na.rm = TRUE)
        ) |>
        dplyr::filter(kid2019 != "-9") |>
        dplyr::ungroup()

    #--------------------------------------------------
    # merge average prices with district information

    housing_data_prices_sf <- merge(
        housing_data_prices,
        districts,
        by = "kid2019",
        all.x = TRUE
    )

    # set geometry
    housing_data_prices_sf <- sf::st_set_geometry(
        housing_data_prices_sf,
        housing_data_prices_sf$geometry
    )

    #--------------------------------------------------
    # create plotting function for map

    plotting_map <- function(
        housing_data = NA,
        variable_of_interest = NA,
        legend_name = NA,
        breaks_steps = NA,
        file_name = NA
    ) {
        map <- ggplot()+
            geom_sf(
                data = housing_data,
                mapping = aes(fill = .data[[variable_of_interest]])
            )+
            scale_fill_viridis_c(
                option = "magma",
                direction = -1,
                breaks = seq(
                    0,
                    max(housing_data[[variable_of_interest]], na.rm = TRUE),
                    by = breaks_steps
                ),
                limits = c(
                    0,
                    max(housing_data[[variable_of_interest]], na.rm = TRUE)
                ),
                na.value = "gray90",
                name = legend_name,
                labels = scales::comma
            )+
            theme_void()+
            theme(
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 13)
            )

        ggsave(
            plot = map,
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["current_version"]],
                "maps",
                paste0(file_name, ".png")
            ),
            dpi = config_globals()[["owndpi"]]
        )
    }

    for (year in unique(housing_data_prices_sf$ejahr)) {
        plotting_map(
            housing_data = housing_data_prices_sf,
            variable_of_interest = "listing_price_sqm",
            legend_name = "Listing price per sqm. (EUR)",
            breaks_steps = 2000,
            file_name = paste0("listing_price_sqm_", year)
        )
        
        plotting_map(
            housing_data = housing_data_prices_sf,
            variable_of_interest = "rent_per_sqm",
            legend_name = "Rent per sqm. (EUR)",
            breaks_steps = 4,
            file_name = paste0("rent_per_sqm_", year)
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}