
reading_geo_data <- function(
    spatial_unit = NA,
    year = NA,
    ags_name = NA,
    gen_name = NA
) {
    #' @title Read in district shape data
    #' 
    #' @description This function reads in the different shape data.
    #' 
    #' @param spatial_unit Indicator for spatial unit
    #' @param year Year of district shape data
    #' @param ags_name How AGS should be renamed after reading
    #' @param gen_name How GEN should be renamed after reading
    #' 
    #' @return Spatial dataframe
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # abbreviation for district or municipality

    if (spatial_unit == "Kreis") {
        filename <- "VG250_KRS"
        region <- spatial_unit
    } else if (spatial_unit == "Gemeinde") {
        filename <- "VG250_GEM"
        region <- spatial_unit
    } else {
        filename <- "PLZ"
        region <- "Postleitzahl"
    }

    #--------------------------------------------------
    # read data

    dta <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            region,
            year,
            paste0(filename, ".shp")
        ),
        quiet = TRUE
    )

    #--------------------------------------------------
    # cleaning

    if (spatial_unit != "PLZ") {
        dta <- dta |>
            # keep only AGS and geometry columns
            dplyr::select(
                AGS, GEN, geometry
            ) |>
            # rename AGS
            dplyr::rename(
                !!rlang::sym(ags_name) := AGS,
                !!rlang::sym(gen_name) := GEN
            )
    } else {
        dta <- dta |>
            dplyr::rename(
                !!rlang::sym(ags_name) := PLZ
            )
    }

    # convert factors
    dta <- dta |>
        dplyr::mutate_if(is.factor, as.character)

    # transform to UTM
    dta <- sf::st_transform(
        dta,
        crs = config_globals()[["utmcrs"]]
    )

    #----------------------------------------------
    # return

    return(dta)
}