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
        "not_used_anymore" = -6,
        "not_specified" = -7,
        "other" = -9,
        "immo_missing" = -1, # how ImmoScout defines missings
        # how not specified appear in the data (in German)
        "not_specified_variants" = c("keine Angabe", "Keine Angabe", "keine Angaben")
    )

    #--------------------------------------------------
    # return

    return(missings)
}