translating_variables <- function(
    housing_data = NA
) {
    #' @title Translating variables
    #' 
    #' @description This function translates variables from German to English.
    #' 
    #' @param housing_data Dataframe with housing data
    #' 
    #' @return Dataframe with translated variables
    #' @author Patrick Thiel

    #--------------------------------------------------
    # Variables that should not be translated

    # extract bef variables
    # NOTE: Their number can vary across waves
    bef_variables <- housing_data |>
        dplyr::select(dplyr::contains("bef")) |>
        names()

    # list all variables that should not be translated
    not_translate_variables <- c(
        "obid",
        "geox",
        "geoy",
        "duplicateid",
        "redc_version",
        "redc_delivery",
        "ajahr",
        "amonat",
        "ejahr",
        "emonat",
        bef_variables,
        "lon_gps",
        "lat_gps",
        "lon_utm",
        "lat_utm",
        "kname2019",
        "gname2019",
        "ergg_1km",
        "blid",
        "gid2019",
        "kid2019",
        "spell",
        "uniqueID_gen",
        "dupID_gen"
    )

    #--------------------------------------------------
    # Define variables to be translated

    translate_variables <- c(
        "freiab",
        "plz",
        "strasse",
        "hausnr",
        "immobilientyp",
        "objektzustand",
        "ausstattung",
        "mietekalt",
        "nebenkosten",
        "courtage",
        "kaufpreis",
        "baujahr",
        "letzte_modernisierung",
        "grundstuecksflaeche",
        "nutzflaeche",
        "etage",
        "denkmalobjekt",
        "aufzug",
        "keller",
        "parkplatz",
        "rollstuhlgerecht",
        "heizungsart",
        "energieausweistyp",
        "ev_kennwert",
        "energieeffizienzklasse",
        "laufzeittage",
        "miete_proqm",
        "teilbar_ab",
        "anbieter",
        "kategorie_business",
        "nebenkosten_proqm",
        "mietekaution_price",
        "mietekaution_type"
    )

    #--------------------------------------------------
    # Test that all variables are included

    all_defined_variable <- c(
        not_translate_variables,
        translate_variables
    )

    for (var in all_defined_variable) {
        targets::tar_assert_true(
            var %in% names(housing_data),
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable {var} is not in housing data.",
                " (Error code: tv#1)"
            )
        )
    }

    #--------------------------------------------------
    # Define translations
    # NOTE: Order has to match the order of translate_variables

    translations <- c(
        "freiab" = "available_from",
        "plz" = "zipcode",
        "strasse" = "street",
        "hausnr" = "house_number",
        "immobilientyp" = "property_type",
        "objektzustand" = "property_condition",
        "ausstattung" = "endowment",
        "mietekalt" = "cold_rent",
        "nebenkosten" = "ancillary_costs",
        "courtage" = "commission",
        "kaufpreis" = "listing_price",
        "baujahr" = "construction_year",
        "letzte_modernisierung" = "last_modernization",
        "grundstuecksflaeche" = "plot_area",
        "nutzflaeche" = "usable_area",
        "etage" = "floor",    
        "denkmalobjekt" = "protected_building",
        "aufzug" = "elevator",
        "keller" = "basement",
        "parkplatz" = "parking",
        "rollstuhlgerecht" = "wheelchair_accessible",
        "heizungsart" = "heating_type",
        "energieausweistyp" = "energy_certificate_type",
        "ev_kennwert" = "energy_consumption_index",
        "energieeffizienzklasse" = "energy_efficiency_class",
        "laufzeittage" = "duration_days",
        "miete_proqm" = "rent_per_sqm",
        "teilbar_ab" = "divisible_from",
        "anbieter" = "provider",
        "kategorie_business" = "category_business",
        "nebenkosten_proqm" = "ancillary_costs_per_sqm",
        "mietekaution_price" = "security_deposit_price",
        "mietekaution_type" = "security_deposit_type"
    )

    # Check that translated variables and translations match
    targets::tar_assert_true(
        length(translate_variables) == length(translations),
        msg = glue::glue(
            "!!! WARNING: ",
            "Number of variables to translate and translations do not match.",
            " (Error code: tv#2)"
        )
    )

    # Check that order of translate_variables and translations match
    targets::tar_assert_true(
        all(translate_variables == names(translations)),
        msg = glue::glue(
            "!!! WARNING: ",
            "Order of variables to translate and translations do not match.",
            " (Error code: tv#3)"
        )
    )

    # Apply translations
    for (i in 1:length(translate_variables)) {
        if (translate_variables[i] %in% names(housing_data)) {
            names(housing_data)[names(housing_data) == translate_variables[i]] <- translations[translate_variables[i]]
        } else {
            targets::tar_error(
                msg = glue::glue(
                    "!!! WARNING: ",
                    "Variable '{translate_variables[i]}' not found in housing data.",
                    " (Error code: tv#2)"
                )
            )
        }
    }

    #--------------------------------------------------
    # return

    return(housing_data)
}