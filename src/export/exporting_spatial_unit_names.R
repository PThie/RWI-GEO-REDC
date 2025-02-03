exporting_spatial_unit_names <- function(
    spatial_units = NA
) {
    #' @title Exporting spatial unit names
    #' 
    #' @description This function exports the names and corresponding IDs for
    #' the different spatial units for reporting and labeling.
    #' 
    #' @param spatial_units Spatial dataframe of spatial units.
    #' 
    #' @return Dataframe with spatial unit names and IDs.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # remove geometry

    spatial_units_df <- sf::st_drop_geometry(spatial_units)

    #--------------------------------------------------
    # keep only distinct values

    spatial_units_df <- dplyr::distinct(spatial_units_df)

    #--------------------------------------------------
    # export

    filename <- paste0(
        "spatial_units_names_",
        names(spatial_units_df)[2],
        ".xlsx"
    )

    openxlsx::write.xlsx(
        spatial_units_df,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            filename
        ),
        rowNames = FALSE
    )

    #--------------------------------------------------
    # return

    return(spatial_units_df)
}