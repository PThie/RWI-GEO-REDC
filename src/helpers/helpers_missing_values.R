helpers_missing_values <- function() {
    #' @title Define missing values
    #' 
    #' @description This function defines the missing values for the data set.
    #' 
    #' @return List of missing values
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define missing values

    missings <- list(
        "implausible" = -5,
        "not_used_anymore" = -6, # "Not available for this delivery"
        "not_specified" = -7,
        "other" = -9,
        "censored" = -11,
        "immo_missing" = -1, # how ImmoScout defines missings
        # how not specified appear in the data (in German)
        "not_specified_variants" = c("keine Angabe", "Keine Angabe", "keine Angaben"),
        "all_missings" = c(-5, -6, -7, -9, -11)
    )

    #--------------------------------------------------
    # return

    return(missings)
}