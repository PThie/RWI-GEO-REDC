helpers_implausible_values <- function() {
    #' @title Define implausible values
    #' 
    #' @description This function defines the implausible values for the data set.
    #' 
    #' @return List of implausible values
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define implausible values
    
    ##### last renovation (letzte modernisierung)
    # censor if last renovation is far back (before 1800)
    last_renovation_max_value <- 1800

    ##### construction year (baujahr)
    # censor if construction year is far back (before 1000)
    construction_year_max_value <- 1000

    #--------------------------------------------------
    # combine all

    implausible_values <- list(
        "last_renovation_max_value" = last_renovation_max_value,
        "construction_year_max_value" = construction_year_max_value
    )

    #--------------------------------------------------
    # return

    return(implausible_values)
}