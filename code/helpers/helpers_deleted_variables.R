helpers_deleted_variables <- function() {
    #' @title Define variables to be removed
    #' 
    #' @description This function defines the variables to be removed from the
    #' data set that are potentially in the data set because the data were
    # delivered with the residential data together
    #'
    #' @return Vector with variables to be removed
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define variables to be removed
    # TODO NEW WAVE: Update variables to be removed (if necessary)
    
    del_vars <- c(
        "mieteinnahmenpromonat",
        "schlafzimmer",
        "badezimmer",
        "gaestewc",
        "einliegerwohnung",
        "ferienhaus",
        "einbaukueche",
        "balkon",
        "garten",
        "foerderung",
        "parkplatzpreis",
        "wohngeld",
        "mietewarm",
        "kaufvermietet",
        "heizkosten",
        "anzahletagen",
        "is24_stadt_kreis", # dropped in RED as well
        "kreis",
        "is24_bezirk_gemeinde", # dropped in RED as well
        "is24_bundesland", # dropped in RED as well
        "skid", # dropped in RED as well   
        "bgid", # dropped in RED as well
        "objekt_beschreibung", # text description of the add removed for now
        "betreut", # not used anymore
        "nebenraeume", # not used in REDC
        "einstelldatum", # not needed for now
        "version" # not needed and can be easily confused with redc_version
    )

    #--------------------------------------------------
    # return
    
    return(del_vars)
}