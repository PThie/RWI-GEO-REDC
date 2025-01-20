helpers_dupID_variables <- function() {
    #' @title Define variables for the dupID generation
    #' 
    #' @description This function defines the variables that are used for the
    #' generation of the dupID. The variables are split into set 1 (fixed
    #' variables) and set 2 (flexible variables).
    #' 
    #' @return List with the variables
    #' @author Patrick Thiel
    #' 
    #' @note Split into continuous and categorical variables because the
    #' algorithm determines the difference between the current and the previous
    #' spell and uses percent differences for continuous variables and logical
    #' differences for categorical variables.

    #--------------------------------------------------
    vars <- list(
        #--------------------------------------------------
        # definition of set 1 variables (fixed variables)
        "continuous_set_1" = c(
            "grundstuecksflaeche"
        ),
        "categorical_set_1" = c(
            "baujahr",
            "etage",
            "plz"
        ),
        #--------------------------------------------------
        # definition of set 2 variables (flexible variables)
        "continuous_set_2" = c(
            "kaufpreis",
            "mietekalt"
        ),
        "categorical_set_2" = c(
            "ausstattung",
            "kategorie_business",
            "energieeffizienzklasse",
            "heizungsart",
            "objektzustand"
        ),
        #--------------------------------------------------
        # definition of variables where the deviation to previous spell cannot
        # exceed 20% or 10%
        "critical_variables_20_perc" = c(
            "kaufpreis",
            "mietekalt"
        ),
        "critical_variables_10_perc" = (
            "grundstuecksflaeche"
        )
    )

    #--------------------------------------------------
    # return

    return(vars)
}