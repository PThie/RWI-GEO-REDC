reading_org_data <- function(redc_raw_file = NA) {
    #' @title Reading original data
    #' 
    #' @description This function reads the raw data.
    #' 
    #' @param redc_raw_file File path to raw/ original data
    #' 
    #' @note Separating the reading process from the cleaning process avoids
    #' that the original data has to be reloaded multiple times.
    #' 
    #' @return DataFrame
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # read data
    
    org_data <- data.table::fread(
        redc_raw_file,
        encoding = "UTF-8"
    )

    #--------------------------------------------------
    # check that dataframe is not empty

    targets::tar_assert_nonempty(
        org_data,
        msg = glue::glue(
            "!!! WARNING:",
            " Dataset is empty.",
            " (Error code: rod#1)"
        )
    )

    #----------------------------------------------
    # return

    return(org_data)
}