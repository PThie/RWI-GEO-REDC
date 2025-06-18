testing_temporal_trends <- function(
    housing_data = NA
) {
    #' @title Test temporal trends
    #' 
    #' @description This function tests the temporal trends of the housing market
    #' by plotting the evolution of prices and rents over time.
    #' 
    #' @param housing_data Dataframe with the housing data
    #' 
    #' @return ggplot object with lineplot
    #' @author Patrick Thiel

    #--------------------------------------------------
    # set language to English
    # NOTE: needed for month names

    Sys.setlocale("LC_ALL", "English")

    #--------------------------------------------------
    # add year-month

    housing_data_prep <- housing_data |>
        dplyr::mutate(
            year_month = paste0(ejahr, "-", emonat),
            year_month = zoo::as.yearmon(year_month, "%Y-%m"),
            year_quarter = zoo::as.yearqtr(year_month)
        )

    #--------------------------------------------------
    # set proper NA

    housing_data_prep <- housing_data_prep |>
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
    # summarize prices and rents

    housing_data_prices <- housing_data_prep |>
        dplyr::mutate(
            listing_price_sqm = listing_price / usable_area
        ) |>
        dplyr::group_by(year_quarter) |>
        dplyr::summarize(
            listing_price_sqm = mean(listing_price_sqm, na.rm = TRUE),
            rent_per_sqm = mean(rent_per_sqm, na.rm = TRUE)
        ) |>
        dplyr::ungroup()

    #--------------------------------------------------
    # generate plot

    linewidth_local <- 1.2
    size_local <- 3

    brks <- c(
        unique(housing_data_prices$year_quarter)[
            stringr::str_detect(housing_data_prices$year_quarter, "Q1")
        ],
        zoo::as.yearqtr(paste(
            config_globals()[["max_year"]] + 1,
            "Q1"
        ))
    )

    lineplot <- ggplot(
        data = housing_data_prices,
        aes(x = year_quarter)
    )+
        geom_line(
            mapping = aes(
                y = listing_price_sqm,
                color = "prices"
            ),
            linewidth = 1.2
        )+
        geom_point(
            mapping = aes(
                y = listing_price_sqm,
                color = "prices"
            ),
            size = size_local
        )+
        geom_line(
            mapping = aes(
                y = rent_per_sqm * 150,
                color = "rents"
            ),
            linewidth = linewidth_local
        )+
        geom_point(
            mapping = aes(
                y = rent_per_sqm * 150,
                color = "rents"
            ),
            size = size_local
        )+
        scale_y_continuous(
            name = "Listing price per sqm. (EUR)",
            sec.axis = sec_axis(
                ~ .x / 150,
                labels = scales::comma,
                name = "Rents per sqm. (EUR)"
            ),
            breaks = seq(0, max(housing_data_prices$listing_price_sqm) + 1000, by = 1000),
            limits = c(0, max(housing_data_prices$listing_price_sqm) + 1000),
            labels = scales::comma
        )+
        zoo::scale_x_yearqtr(
            labels = scales::label_date(format = "%Y-%q"),
            breaks = brks,
            name = ""
        )+
        scale_color_manual(
            values = c(
                "prices" = config_globals()[["java_five_colors"]][3],
                "rents" = config_globals()[["java_five_colors"]][5]
            ),
            labels = c(
                "prices" = "Listing price per sqm.",
                "rents" = "Rent per sqm."
            ),
            name = ""
        )+
        theme_light()+
        theme(
            panel.grid.minor.x = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 14),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 14),
            axis.title.y = element_text(vjust = 2.5),
            axis.text.x = element_text(angle = 90) 
        )

    ggsave(
        plot = lineplot,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "graphs",
            "temporal_trends.png"
        ),
        dpi = config_globals()[["owndpi"]]
    )

    #--------------------------------------------------
    # return

    return(lineplot)
}