helpers_deleted_variables_SUF <- function() {
    #' @title Defining variables to exclude from SUF file
    #' 
    #' @description This function defines variables that should be excluded from
    #' the SUF file.
    #' 
    #' @return Vector with variables to exclude
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define variables that should be removed in SUF file

    delete_vars <- c(
        "geox",
        "geoy",
        "lat_gps",
        "lon_gps",
        "lat_utm",
        "lon_utm",
        "available_from",
        "street",
        "house_number",
        "commission"
    )

    #--------------------------------------------------
    # return

    return(delete_vars)
}