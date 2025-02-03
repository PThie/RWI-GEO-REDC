#----------------------------------------------
# path handling

config_paths <- function() {
    main_path <- "M:/_FDZ/RWI-GEO/RWI-GEO-REDC"

    data_path <- file.path(
        main_path,
        "data"
    )

    code_path <- file.path(
        main_path,
        "code"
    )

    logs_path <- file.path(
        main_path,
        "logs"
    )

    gebiete_path <- file.path(
        "M:/_FDZ/interne Daten/Gebietseinheit"
    )
    
    output_path <- file.path(
        main_path,
        "output"
    )

    microm_data_path <- file.path(
        "M:/_FDZ/RWI-GEO/RWI-GEO-GRID/daten/original/Stata16"
    )

    # combine all
    all_paths <- list(
        "main_path" = main_path,
        "data_path" = data_path,
        "code_path" = code_path,
        "logs_path" = logs_path,
        "gebiete_path" = gebiete_path,
        "output_path" = output_path,
        "microm_data_path" = microm_data_path
    )

    #--------------------------------------------------
    # return

    return(all_paths)
}

#----------------------------------------------
# globals

config_globals <- function() {
    # define current delivery
    # TODO NEW WAVE: Update delivery number
    current_delivery <- "Lieferung_2306"

    # counter for delivery
    # mainly used for delivery variable
    # TODO NEW WAVE: Update delivery counter
    current_delivery_counter <- 1

    # list of deliveries
    # TODO NEW WAVE: Update delivery numbers
    # deliveries <- c(2306, 2312, 23121)
    deliveries <- c(2306)

    # current version of REDC
    # TODO NEW WAVE: Update version number
    current_version <- "v1"

    # previous version of REDC
    previous_version <- paste0(
        "v",
        as.numeric(
            substring(current_version, 2, nchar(current_version))
        ) - 1
    )

    # maximum year in the current delivery
    # TODO NEW WAVE: Update max year
    max_year <- 2023

    # CRS UTM (refers to ETRS89 / UTM zone 32N)
    # https://epsg.io/25832
    utmcrs <- 25832
    # CRS GPS (refers to WGS 84 -World Geodetic System 1984, used in GPS)
    # https://epsg.io/4326
    gpscrs <- 4326

    # data folders
    data_folders <- c("on-site", "processed", "SUF")

    # exported file formats (of the final data)
    # NOTE: needed for the folder structure
    exported_file_formats <- c("parquet", "csv")

    # ImmoScout projection
    projection_immo <- "+proj=lcc +lat_1=40 +lat_2=60 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

    # version of RWI-GEO-GRID
    # TODO NEW WAVE: Update version number (if there is a new GRID version)
    microm_data_version <- "v14"

    # max year in microm data
    microm_max_year <- 2022

    # censoring threshold
    # NOTE: Number of business in a grid cell (if undercut, the ergg_1km value is masked)
    censoring_threshold_businesses <- 5

    # combine all
    all_globals <- list(
        "current_delivery" = current_delivery,
        "current_delivery_counter" = current_delivery_counter,
        "deliveries" = deliveries,
        "current_version" = current_version,
        "previous_version" = previous_version,
        "max_year" = max_year,
        "utmcrs" = utmcrs,
        "gpscrs" = gpscrs,
        "data_folders" = data_folders,
        "exported_file_formats" = exported_file_formats,
        "projection_immo" = projection_immo,
        "microm_data_version" = microm_data_version,
        "censoring_threshold_businesses" = censoring_threshold_businesses,
        "microm_max_year" = microm_max_year
    )

    #--------------------------------------------------
    # return

    return(all_globals)
}

#----------------------------------------------
# variable that should be removed because of data securities
# TODO: Move to a helper function

config_confidential_variables <- function() {
    conf_vars <- c(
        "ort",
        "strasse",
        "hausnr",
        "lat_gps",
        "lon_gps",
        "lat_utm",
        "lon_utm"
    )

    return(conf_vars)
}

#--------------------------------------------------
# variables that have been fixed
# NOTE: add the variables that have been fixed
# Needed in testing_consistent_variables.R (otherwise pipeline stops)

config_fixed_variables <- function() {
    fixed_vars <- list(
        "fixed_names_vars" = c(

        ),
        "fixed_types_vars" = c(

        )
    )
    return(fixed_vars)
}

#--------------------------------------------------
# variables to drop for SUF
