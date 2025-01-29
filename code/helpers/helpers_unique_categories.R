helpers_unique_categories <- function() {
    #' @title Unique categories
    #' 
    #' @description This function defines the number of unique categories for each
    #' categorical variable.
    #' 
    #' @note Needed to generate value labels for categorical variables.
    #' 
    #' @return List with unique values for each categorical variable.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # define length of unique categories and the type of categorical variable
    # NOTE: Exclude missings from count

    cats <- list(
        "basement" = list(
            "unique_values" = 2,
            "type" = "dummy"
        ) ,
        "category_business" = list(
            "unique_values" = 53,
            "type" = "categorical"
        ),
        "dupID_gen" = list(
            "unique_values" = 9,
            "type" = "categorical"
        ),
        "elevator" = list(
            "unique_values" = 2,
            "type" = "dummy"
        ),
        "endowment" = list(
            "unique_values" = 4,
            "type" = "categorical"
        ),
        "energy_certificate_type" = list(
            "unique_values" = 2,
            "type" = "categorical"
        ),
        "energy_efficiency_class" = list(
            "unique_values" = 9,
            "type" = "categorical"
        ),
        "heating_type" = list(
            "unique_values" = 13,
            "type" = "categorical"
        ),
        "parking" = list(
            "unique_values" = 2,
            "type" = "dummy"
        ),
        "property_condition" = list(
            "unique_values" = 10,
            "type" = "categorical"
        ),
        "property_type" = list(
            "unique_values" = 5,
            "type" = "categorical"
        ),
        "protected_building" = list(
            "unique_values"= 2,
            "type" = "dummy"
        ),
        "provider" = list(
            "unique_values" = 8,
            "type" = "categorical"
        ),
        "security_deposit_type" = list(
            "unique_values" = 6,
            "type" = "categorical"
        ),
        "wheelchair_accessible" = list(
            "unique_values"= 2,
            "type" = "dummy"
        )
    )

    #--------------------------------------------------
    # return

    return(cats)
}