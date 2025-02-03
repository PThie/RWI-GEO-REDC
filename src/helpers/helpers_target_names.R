helpers_target_names <- function() {
    #' @title Create target names
    #' 
    #' @description This function creates a list of target names used in the
    #' pipeline when dynamic branching is used (i.e. when tar_eval is used).
    #'  
    #' @return List, target names
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # list of target names

    # NOTE: define outside list to be able to use in list
    spatial_units_english <- c("district", "municipality", "zip_code")
    exported_file_formats <- c("parquet", "csv")

    target_names <- list(
        #--------------------------------------------------
        # names for the spatial information
        # NOTE: order has to match either other
        "spatial_units_german" = c("Kreis", "Gemeinde", "PLZ"),
        "ags_names" = c("kid2019", "gid2019", "plz2019"),
        # NOTE: zip code does not have a GEN name
        "gen_names" = c("kname2019", "gname2019", NA),
        # name for the target objects
        "spatial_data" = glue::glue(
            "spatial_data_{spatial_units_english}"
        ),
        "spatial_unit_names" = glue::glue(
            "spatial_unit_names_{spatial_units_english}"
        ),
        #--------------------------------------------------
        # names for testing
        "exported_file_formats" = exported_file_formats,
        "suf_exported_data" = glue::glue(
            "suf_exported_data_{exported_file_formats}"
        ),
        "suf_compliance_test" = glue::glue(
            "suf_compliance_test_{exported_file_formats}"
        )
    )
    
    #--------------------------------------------------
    # return

    return(target_names)
}
