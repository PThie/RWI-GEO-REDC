testing_SUF_compliance <- function(
    suf_data = NA,
    file_format = NA
) {
    #' @title Test SUF compliance
    #' 
    #' @description This function tests if the SUF data is compliant with the
    #' requirements, i.e. if all sensitive variables have been deleted.
    #' 
    #' @param suf_data Dataframe with the SUF data
    #' @param file_format File format of the SUF data (character)
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # test if all variables have been deleted that are sensitive

    for (col in helpers_deleted_variables_SUF()) {
        targets::tar_assert_true(
            !(col %in% names(suf_data)),
            msg = glue::glue(
                "!!! WARNING: ",
                "Sensitive variable '{col}' is still present in the {file_format} data.",
                " (Error code: sct#1)"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}