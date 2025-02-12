creating_variable_labels <- function(
    housing_data = NA
) {
    #' @title Generating variable labels
    #' 
    #' @description This function generates labels for variables for documentation.
    #' 
    #' @param housing_data Dataframe with housing data
    #' 
    #' @return Dataframe with variable labels
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract variable names

    var_name <- names(housing_data) |>
        sort()

    # write into dataframe
    label_data <- data.frame(
        "variable" = var_name
    )

    #--------------------------------------------------
    # create category
    # NOTE: similar to RED

    label_data <- label_data |>
        dplyr::mutate(
            category = NA_character_,
            category = dplyr::case_when(
                variable %in% c(
                    "basement",
                    "elevator",
                    "endowment",
                    "floor",
                    "parking",
                    "protected_building",
                    "wheelchair_accessible"
                ) ~ "Specific features",
                variable %in% c(
                    "category_business",
                    "property_type",
                    "provider"
                ) ~ "General features",
                variable %in% c(
                    "dupID_gen",
                    "spell",
                    "duplicateid",
                    "redc_delivery",
                    "redc_version"
                ) ~ "Generated features",
                variable %in% c(
                    "obid",
                    "uniqueID_gen"
                ) ~ "Identifiers",
                variable %in% c(
                    "blid",
                    "ergg_1km",
                    "geox",
                    "geoy",
                    "gid2019",
                    "gname2019",
                    "house_number",
                    "kid2019",
                    "kname2019",
                    "lat_gps",
                    "lat_utm",
                    "lon_gps",
                    "lon_utm",
                    "street",
                    "zipcode"
                ) ~ "Regional information",
                variable %in% c(
                    "ajahr",
                    "amonat",
                    "available_from",
                    "ejahr",
                    "emonat",
                    "duration_days"
                ) ~ "Temporal information",
                variable %in% c(
                    "construction_year",
                    "energy_certificate_type",
                    "energy_consumption_index",
                    "energy_efficiency_class",
                    "heating_type",
                    "last_modernization",
                    "property_condition"
                ) ~ "Features related to energy and building structure",
                grepl("bef", variable) ~ "Features related to energy and building structure",
                variable %in% c(
                    "ancillary_costs",
                    "ancillary_costs_per_sqm",
                    "cold_rent",
                    "commission",
                    "listing_price",
                    "rent_per_sqm",
                    "security_deposit_months",
                    "security_deposit_price",
                    "security_deposit_type"
                ) ~ "Features related to price",
                variable %in% c(
                    "divisible_from",
                    "plot_area",
                    "usable_area"
                ) ~ "Features related to size",
            )
        )

        # Check that no variable was not classified
        targets::tar_assert_true(
            length(which(is.na(label_data$category))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not classified.",
                " (Error code: cvl#1)"
            )
        )

        #--------------------------------------------------
        # create labels

        label_data <- label_data |>
            dplyr::mutate(
                label = NA_character_,
                label = dplyr::case_when(
                    variable == "ajahr" ~ "Start of offer (year)",
                    variable == "amonat" ~ "Start of offer (month)",
                    variable == "ancillary_costs" ~ "Ancillary costs (EUR)",
                    variable == "ancillary_costs_per_sqm" ~ "Ancillary costs per square meter (EUR)",
                    variable == "available_from" ~ "Available from",
                    variable == "basement" ~ "Indicator for basement",
                    variable == "blid" ~ "Federal state ID",
                    variable == "category_business" ~ "Category business",
                    variable == "cold_rent" ~ "Cold rent (excluding heating costs) (EUR)",
                    variable == "commission" ~ "Commission (Brokerage fee) (EUR)",
                    variable == "construction_year" ~ "Construction year",
                    variable == "divisible_from" ~ "Divisible from (sq. meter)",
                    variable == "dupID_gen" ~ "Classification of duplicates (RWI)",
                    variable == "duplicateid" ~ "Related obid (ImmoScout)",
                    variable == "duration_days" ~ "Duration in days",
                    variable == "ejahr" ~ "End of offer (year)",
                    variable == "emonat" ~ "End of offer (month)",
                    variable == "elevator" ~ "Indicator for elevator",
                    variable == "endowment" ~ "Endowment",
                    variable == "energy_certificate_type" ~ "Energy certificate type",
                    variable == "energy_consumption_index" ~ "Energy consumption index",
                    variable == "energy_efficiency_class" ~ "Energy efficiency class",
                    variable == "ergg_1km" ~ "1 sq. km raster cell following INSPIRE",
                    variable == "floor" ~ "Floor",
                    variable == "geox" ~ "Geographical coordinate longitude (ImmoScout)",
                    variable == "geoy" ~ "Geographical coordinate latitude (ImmoScout)",
                    variable == "gid2019" ~ "Municipality ID 2019 (AGS)",
                    variable == "gname2019" ~ "Municipality name 2019",
                    variable == "heating_type" ~ "Heating type",
                    variable == "house_number" ~ "House number",
                    variable == "kname2019" ~ "District name 2019",
                    variable == "kid2019" ~ "District ID 2019",
                    variable == "lat_gps" ~ "Geographical coordinate latitude GPS",
                    variable == "lat_utm" ~ "Geographical coordinate latitude UTM",
                    variable == "last_modernization" ~ "Last modernization (year)",
                    variable == "listing_price" ~ "Listing price (EUR)",
                    variable == "lon_gps" ~ "Geographical coordinate longitude GPS",
                    variable == "lon_utm" ~ "Geographical coordinate longitude UTM",
                    variable == "obid" ~ "Property unit identifier",
                    variable == "parking" ~ "Indicator for parking",
                    variable == "plot_area" ~ "Plot area (sq. meter)",
                    variable == "property_condition" ~ "Property condition",
                    variable == "property_type" ~ "Property type",
                    variable == "provider" ~ "Provider of ad",
                    variable == "protected_building" ~ "Indicator for protected building",
                    variable == "redc_delivery" ~ "Indicator for RWI-GEO-REDC delivery",
                    variable == "redc_version" ~ "Indicator for RWI-GEO-REDC version",
                    grepl("bef", variable) ~ "Indicator for firing type",
                    variable == "rent_per_sqm" ~ "Rent per square meter (EUR)",
                    variable == "security_deposit_months" ~ "Security deposit in months",
                    variable == "security_deposit_price" ~ "Security deposit price (EUR)",
                    variable == "security_deposit_type" ~ "Security deposit type",
                    variable == "spell" ~ "Indicator for spell",
                    variable == "street" ~ "Street",
                    variable == "uniqueID_gen" ~ "Unique ID (RWI)",
                    variable == "usable_area" ~ "Usable area (sq. meter)",
                    variable == "wheelchair_accessible" ~ "Indicator for wheelchair accessibility",
                    variable == "zipcode" ~ "Zip code"
                )
            )

        # Check that no variable was not labeled
        targets::tar_assert_true(
            length(which(is.na(label_data$label))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not labeled.",
                " (Error code: cvl#2)"
            )
        )

        #--------------------------------------------------
        # create variable types

        label_data <- label_data |>
            dplyr::mutate(
                variable_type = NA_character_,
                variable_type = dplyr::case_when(
                    variable %in% c(
                        "ajahr",
                        "amonat",
                        "ancillary_costs",
                        "ancillary_costs_per_sqm",
                        "cold_rent",
                        "construction_year",
                        "divisible_from",
                        "duration_days",
                        "ejahr",
                        "emonat",
                        "energy_consumption_index",
                        "geox",
                        "geoy",
                        "last_modernization",
                        "lat_gps",
                        "lat_utm",
                        "listing_price",
                        "lon_gps",
                        "lon_utm",
                        "plot_area",
                        "rent_per_sqm",
                        "security_deposit_months",
                        "security_deposit_price",
                        "spell",
                        "uniqueID_gen",
                        "usable_area"
                    ) ~ "numeric",
                    variable %in% c(
                        "available_from",
                        "commission",
                        "duplicateid",
                        "ergg_1km",
                        "floor",
                        "gid2019",
                        "gname2019",
                        "house_number",
                        "kid2019",
                        "kname2019",
                        "obid",
                        "redc_version",
                        "street",
                        "zipcode"
                    ) ~ "character",
                    variable %in% c(
                        "basement",
                        "category_business",
                        "dupID_gen",
                        "elevator",
                        "endowment",
                        "energy_certificate_type",
                        "energy_efficiency_class",
                        "heating_type",
                        "parking",
                        "property_condition",
                        "property_type",
                        "protected_building",
                        "provider",
                        "security_deposit_type",
                        "wheelchair_accessible",
                        "blid",
                        "redc_delivery"
                    ) ~ "categorical",
                    grepl("bef", variable) ~ "numeric"
                )
            ) |>
            dplyr::rename(`variable type` = variable_type)

        # Check that no variable was not labeled
        targets::tar_assert_true(
            length(which(is.na(label_data[["variable type"]]))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not assigned.",
                " (Error code: cvl#3)"
            )
        )

        #--------------------------------------------------
        # export

        openxlsx::write.xlsx(
            label_data,
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["current_version"]],
                "info",
                "variable_labels.xlsx"
            ),
            rowNames = FALSE
        )

        #--------------------------------------------------
        # return

        return(label_data)
}