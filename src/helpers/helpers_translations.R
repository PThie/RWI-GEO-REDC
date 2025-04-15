helpers_translations <- function() {
    #' @title Translation of variable names
    #'
    #' @description This function lists the translated variable names from the
    #' original data to the English version.
    #' 
    #' @return Vector of translated variable names
    #' @author Patrick Thiel
    
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
        "mietekaution_type" = "security_deposit_type",
        "mietekaution_months" = "security_deposit_months",
        "heizkosten" = "heating_costs",
        "heizkosten_in_wm_enthalten" = "heating_costs_included_rent",
        "ev_wwenthalten" = "warm_water_cons_included_energy_cons",
        "hits" = "num_hits_ad",
        "click_schnellkontakte" = "num_clicks_contact_button",  
        "liste_show" = "num_hits_result_list",
        "liste_match" = "num_hits_search_request",     
        "click_weitersagen" = "num_clicks_share_button",
        "click_url" = "num_clicks_url"
    )

    #--------------------------------------------------
    # return

    return(translations)
}
