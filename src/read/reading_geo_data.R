
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

    if (spatial_unit == "AMR") {
        dta <- sf::st_read(
            file.path(
                config_paths()[["gebiete_path"]],
                "Arbeitsmarktregion",
                "RWI2018",
                "shapefiles",
                "amr2.gpkg"
            )
        )
    } else {
        dta <- sf::st_read(
            file.path(
                config_paths()[["gebiete_path"]],
                region,
                year,
                paste0(filename, ".shp")
            ),
            quiet = TRUE
        )
    }

    #--------------------------------------------------
    # cleaning

    if (!spatial_unit %in% c("PLZ", "AMR")) {
        dta <- dta |>
            # keep only AGS and geometry columns
            dplyr::select(
                AGS, GEN, geometry
            ) |>
            # take care of special characters
            dplyr::mutate(
                GEN = stringi::stri_trans_general(
                    GEN,
                    "de-ASCII; Latin-ASCII"
                )
            ) |>
            # rename AGS
            dplyr::rename(
                !!rlang::sym(ags_name) := AGS,
                !!rlang::sym(gen_name) := GEN
            )
    } else if (spatial_unit == "PLZ"){
        dta <- dta |>
            dplyr::rename(
                !!rlang::sym(ags_name) := PLZ
            )
    } else {
        dta <- dta |>
            dplyr::mutate(
                    AMR2 = as.character(AMR2),
                    AMR2 = stringr::str_pad(AMR2, 5, pad = "0")
                ) |>
            dplyr::rename(
                !!rlang::sym(ags_name) := AMR2
            )

        sf::st_geometry(dta) <- "geometry"
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