creating_value_labels <- function(
    housing_data = NA,
    variable_labels = NA
) {
    #' @title Create value labels for categorical variables
    #' 
    #' @description This function creates value labels for categorical variables.
    #' 
    #' @param housing_data Dataframe with housing data.
    #' @param variable_labels Dataframe with variable labels.
    #' 
    #' @return Dataframe with value labels for categorical variables.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # subset housing data for categorical variables

    categorical_data <- housing_data |>
        dplyr::select(
            variable_labels |>
                dplyr::filter(`variable type` == "categorical") |>
                dplyr::pull(variable)
        )

    # extract variable names
    # TODO: this is only a hot fix. bef should be a categorical variable in the
    # data
    # TODO: if you pull bef from names of data, keep in mind that there are multiple
    # bef variables and you only need one (and drop the indexing)
    var_names <- sort(c("bef", names(categorical_data)))

    #--------------------------------------------------
    # create vector of variable names (according to length of unique values)
    # create vector of unique values

    # check that all categorical variables have been assign a maximum value
    for (var in var_names) {
        targets::tar_assert_true(
            var %in% names(helpers_unique_categories()),
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable '{var}' has not been assigned a maximum value.",
                " (Error code: cvl#1)"
            )
        )
    }

    # actual creation
    var_names_rep <- c()
    unique_values <- c()

    for (var in var_names) {
        var_names_rep <- c(
            var_names_rep,
            rep(var, helpers_unique_categories()[[var]]$unique_values)
        )

        # NOTE: dummies and dupID_gen start at 0
        if (helpers_unique_categories()[[var]]$type == "dummy" | var == "dupID_gen") {
            unique_values <- c(
                unique_values,
                seq(0, helpers_unique_categories()[[var]]$unique_values - 1)
            )
        } else {
            unique_values <- c(
                unique_values,
                seq(1, helpers_unique_categories()[[var]]$unique_values)
            )
        }
    }

    # combine into dataframe
    value_labels <- data.frame(
        variable = var_names_rep,
        value = unique_values
    )

    #--------------------------------------------------
    # add labels to the value labels

    value_labels <- value_labels |>
        dplyr::mutate(
            label = NA_character_,
            label = dplyr::case_when(
                #--------------------------------------------------
                variable == "basement" & value == 0 ~ "No",
                variable == "basement" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "bef" & value == 1 ~ "No information",
                variable == "bef" & value == 2 ~ "Geothermal",
                variable == "bef" & value == 3 ~ "Solar heating",
                variable == "bef" & value == 4 ~ "Pellet heating",
                variable == "bef" & value == 5 ~ "Gas heating",
                variable == "bef" & value == 6 ~ "Oil heating",
                variable == "bef" & value == 7 ~ "District heating",
                variable == "bef" & value == 8 ~ "Electricity",
                variable == "bef" & value == 9 ~ "Coal",
                variable == "bef" & value == 10 ~ "Natural gas light",
                variable == "bef" & value == 11 ~ "Natural gas heavy",
                variable == "bef" & value == 12 ~ "Liquid gas",
                variable == "bef" & value == 13 ~ "Steam district heating",
                variable == "bef" & value == 14 ~ "Wood",
                variable == "bef" & value == 15 ~ "Wood chips",
                variable == "bef" & value == 16 ~ "Coal coke",
                variable == "bef" & value == 17 ~ "Local heating",
                variable == "bef" & value == 18 ~ "No attribute assigned",
                variable == "bef" & value == 19 ~ "Heat supply",
                variable == "bef" & value == 20 ~ "Bio energy",
                variable == "bef" & value == 21 ~ "Wind energy",
                variable == "bef" & value == 22 ~ "Hydro energy",
                variable == "bef" & value == 23 ~ "Environmental thermal energy",
                variable == "bef" & value == 24 ~ "Combined heat and power fossil fuels",
                variable == "bef" & value == 25 ~ "Combined heat and power renewable energy",
                variable == "bef" & value == 26 ~ "Combined heat and power regenerative energy",
                variable == "bef" & value == 27 ~ "Combined heat and power bio energy",
                #--------------------------------------------------
                variable == "blid" & value == 1 ~ "Schleswig Holstein",
                variable == "blid" & value == 2 ~ "Hamburg",
                variable == "blid" & value == 3 ~ "Lower Saxony",
                variable == "blid" & value == 4 ~ "Free Hanseatic City of Bremen",
                variable == "blid" & value == 5 ~ "North Rhine-Westphalia",
                variable == "blid" & value == 6 ~ "Hesse",
                variable == "blid" & value == 7 ~ "Rhineland-Palatine",
                variable == "blid" & value == 8 ~ "Baden-Wuerttemberg",
                variable == "blid" & value == 9 ~ "Bavaria",
                variable == "blid" & value == 10 ~ "Saarland",
                variable == "blid" & value == 11 ~ "Berlin",
                variable == "blid" & value == 12 ~ "Brandenburg",
                variable == "blid" & value == 13 ~ "Mecklenburg-Western Pommerania",
                variable == "blid" & value == 14 ~ "The Free State of Saxony",
                variable == "blid" & value == 15 ~ "Saxony-Anhalt",
                variable == "blid" & value == 16 ~ "The Free State of Thuringia",
                #--------------------------------------------------
                variable == "category_business" & value == 1 ~ "Office and commercial buildings",
                variable == "category_business" & value == 2 ~ "Store",
                variable == "category_business" & value == 3 ~ "Living and commercial buildings",
                variable == "category_business" & value == 4 ~ "Sales area",
                variable == "category_business" & value == 5 ~ "Office building",
                variable == "category_business" & value == 6 ~ "Office floors",
                variable == "category_business" & value == 7 ~ "Special property",
                variable == "category_business" & value == 8 ~ "Office center",
                variable == "category_business" & value == 9 ~ "Exhibition space",
                variable == "category_business" & value == 10 ~ "Restaurant",
                variable == "category_business" & value == 11 ~ "Estate",
                variable == "category_business" & value == 12 ~ "Shopping center",
                variable == "category_business" & value == 13 ~ "Cafe",
                variable == "category_business" & value == 14 ~ "Guesthouse",
                variable == "category_business" & value == 15 ~ "Leisure facility",
                variable == "category_business" & value == 16 ~ "Commercial center",
                variable == "category_business" & value == 17 ~ "Hotel",
                variable == "category_business" & value == 18 ~ "Bar service and Lounge",
                variable == "category_business" & value == 19 ~ "Riding school",
                variable == "category_business" & value == 20 ~ "Warehouse with open space",
                variable == "category_business" & value == 21 ~ "Forwarding warehouse",
                variable == "category_business" & value == 22 ~ "High-bay warehouse",
                variable == "category_business" & value == 23 ~ "Club and discotheque",
                variable == "category_business" & value == 24 ~ "Cold storage",
                variable == "category_business" & value == 25 ~ "Department store",
                variable == "category_business" & value == 26 ~ "Factory outlet",
                variable == "category_business" & value == 27 ~ "Vacation bungalow",
                variable == "category_business" & value == 28 ~ "Practice",
                variable == "category_business" & value == 29 ~ "Office",
                variable == "category_business" & value == 30 ~ "Hall",
                variable == "category_business" & value == 31 ~ "Industrial hall",
                variable == "category_business" & value == 32 ~ "Office and warehouse buildings",
                variable == "category_business" & value == 33 ~ "Commercial space",
                variable == "category_business" & value == 34 ~ "Storage space",
                variable == "category_business" & value == 35 ~ "Warehouse",
                variable == "category_business" & value == 36 ~ "Workshop",
                variable == "category_business" & value == 37 ~ "Service area",
                variable == "category_business" & value == 38 ~ "Inn/ Tavern",
                variable == "category_business" & value == 39 ~ "Industrial hall with open space",
                variable == "category_business" & value == 40 ~ "Loft",
                variable == "category_business" & value == 41 ~ "Self-service market",
                variable == "category_business" & value == 42 ~ "Practice floor",
                variable == "category_business" & value == 43 ~ "Lodging house",
                variable == "category_business" & value == 44 ~ "Studio",
                variable == "category_business" & value == 45 ~ "Sales hall",
                variable == "category_business" & value == 46 ~ "Practice building",
                variable == "category_business" & value == 48 ~ "Hotel property",
                variable == "category_business" & value == 49 ~ "Business park",
                variable == "category_business" & value == 47 ~ "Kiosk",
                variable == "category_business" & value == 50 ~ "Farm",
                variable == "category_business" & value == 51 ~ "Hotel garni",
                variable == "category_business" & value == 52 ~ "Refrigerated warehouse",
                variable == "category_business" & value == 53 ~ "Winery",
                #--------------------------------------------------
                variable == "dupID_gen" & value == 0 ~ "First occurence of the ID",
                variable == "dupID_gen" & value == 1 ~ "Identical to 1 and time difference is smaller/ equal 6 months",
                variable == "dupID_gen" & value == 2 ~ "Identical to 1 and time difference is greater 6 months",
                variable == "dupID_gen" & value == 3 ~ "Differences in set 2 variables (below thresholds) and time difference is smaller/ equal 6 months",
                variable == "dupID_gen" & value == 4 ~ "Differences in set 2 variables (below thresholds) and time difference is greater 6 months",
                variable == "dupID_gen" & value == 5 ~ "Differences in set 1 variables (below thresholds) and time difference is smaller/ equal 6 months",
                variable == "dupID_gen" & value == 6 ~ "Differences in set 1 variables (below thresholds) and time difference is greater 6 months",
                variable == "dupID_gen" & value == 7 ~ "Differences in set 1 and 2 variables (below thresholds) and time difference is smaller/ equal 6 months",
                variable == "dupID_gen" & value == 8 ~ "Differences in set 1 and 2 variables (below thresholds) and time difference is greater 6 months",
                variable == "dupID_gen" & value == 9 ~ "Differences exceeding thresholds. Time difference irrelevant",
                #--------------------------------------------------
                variable == "elevator" & value == 0 ~ "No",
                variable == "elevator" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "endowment" & value == 1 ~ "Simple",
                variable == "endowment" & value == 2 ~ "Normal",
                variable == "endowment" & value == 3 ~ "Sophisticated",
                variable == "endowment" & value == 4 ~ "Deluxe",
                #--------------------------------------------------
                variable == "energy_certificate_type" & value == 1 ~ "Energy demand",
                variable == "energy_certificate_type" & value == 2 ~ "Energy usage",
                #--------------------------------------------------
                variable == "energy_efficiency_class" & value == 1 ~ "APLUS",
                variable == "energy_efficiency_class" & value == 2 ~ "A",
                variable == "energy_efficiency_class" & value == 3 ~ "B",
                variable == "energy_efficiency_class" & value == 4 ~ "C",
                variable == "energy_efficiency_class" & value == 5 ~ "D",
                variable == "energy_efficiency_class" & value == 6 ~ "E",
                variable == "energy_efficiency_class" & value == 7 ~ "F",
                variable == "energy_efficiency_class" & value == 8 ~ "G",
                variable == "energy_efficiency_class" & value == 9 ~ "H",
                #--------------------------------------------------
                variable == "heating_type" & value == 1 ~ "Cogeneration or combined heat and power plant",
                variable == "heating_type" & value == 2 ~ "Electric heating",
                variable == "heating_type" & value == 3 ~ "Self-contained central heating",
                variable == "heating_type" & value == 4 ~ "District heating",
                variable == "heating_type" & value == 5 ~ "Floor heating",
                variable == "heating_type" & value == 6 ~ "Gas heating",
                variable == "heating_type" & value == 7 ~ "Wood pellet heating",
                variable == "heating_type" & value == 8 ~ "Night storage heaters",
                variable == "heating_type" & value == 9 ~ "Heating by stove",
                variable == "heating_type" & value == 10 ~ "Oil heating",
                variable == "heating_type" & value == 11 ~ "Solar heating",
                variable == "heating_type" & value == 12 ~ "Thermal heat pump",
                variable == "heating_type" & value == 13 ~ "Central pump",
                #--------------------------------------------------
                variable == "parking" & value == 0 ~ "No",
                variable == "parking" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "property_condition" & value == 1 ~ "First occupancy",
                variable == "property_condition" & value == 2 ~ "First occupancy after refurbishment",
                variable == "property_condition" & value == 3 ~ "Like new",
                variable == "property_condition" & value == 4 ~ "Refurbished",
                variable == "property_condition" & value == 5 ~ "Modernized",
                variable == "property_condition" & value == 6 ~ "Completely renovated",
                variable == "property_condition" & value == 7 ~ "Well-kept",
                variable == "property_condition" & value == 8 ~ "Need of renovation",
                variable == "property_condition" & value == 9 ~ "By arrangement",
                variable == "property_condition" & value == 10 ~ "Dilapidated",
                #--------------------------------------------------
                variable == "property_type" & value == 1 ~ "Office and practice",
                variable == "property_type" & value == 2 ~ "Retail",
                variable == "property_type" & value == 3 ~ "Halls and production",
                variable == "property_type" & value == 4 ~ "Special trades",
                variable == "property_type" & value == 5 ~ "Gastronomy and hotel",
                #--------------------------------------------------
                variable == "protected_building" & value == 0 ~ "No",
                variable == "protected_building" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "provider" & value == 1 ~ "Private",
                variable == "provider" & value == 2 ~ "Broker",
                variable == "provider" & value == 3 ~ "Housing industry",
                variable == "provider" & value == 4 ~ "Property developer",
                variable == "provider" & value == 5 ~ "Financial institution",
                variable == "provider" & value == 6 ~ "Commercial provider",
                variable == "provider" & value == 7 ~ "House construction",
                variable == "provider" & value == 8 ~ "Relocation",
                #--------------------------------------------------
                variable == "redc_delivery" & value == 1 ~ "Jun 2023",
                #--------------------------------------------------
                variable == "security_deposit_type" & value == 1 ~ "Cold rent",
                variable == "security_deposit_type" & value == 2 ~ "Warm rent",
                variable == "security_deposit_type" & value == 3 ~ "Other rent (cold or warm)",
                variable == "security_deposit_type" & value == 4 ~ "Miscellaneous (insurance, credit, gurantee)",
                variable == "security_deposit_type" & value == 5 ~ "Price",
                variable == "security_deposit_type" & value == 6 ~ "No deposit",
                #--------------------------------------------------
                variable == "wheelchair_accessible" & value == 0 ~ "No",
                variable == "wheelchair_accessible" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "heating_costs_included_rent" & value == 0 ~ "No",
                variable == "heating_costs_included_rent" & value == 1 ~ "Yes",
                #--------------------------------------------------
                variable == "warm_water_cons_included_energy_cons" & value == 0 ~ "No",
                variable == "warm_water_cons_included_energy_cons" & value == 1 ~ "Yes",
                #--------------------------------------------------
                # TODO NEW WAVE: Update delivery numbers
                variable == "redc_delivery" & value == 1 ~ "Jun 2023",
                variable == "redc_delivery" & value == 2 ~ "Dec 2023"
            )
        )

    # Check for no NA in values (otherwise missed to specify a label)
    targets::tar_assert_true(
        length(which(is.na(value_labels$label))) == 0,
        msg = glue::glue(
            "!!! WARNING: ",
            "Variable(s) have been missed to specify a label.",
            " (Error code: cvl#2)"
        )
    )

    #--------------------------------------------------
    # export

    openxlsx::write.xlsx(
        value_labels,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "info",
            "value_labels.xlsx"
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(value_labels)
}