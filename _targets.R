#--------------------------------------------------
# description

# This file is the main file that orchestrates the other coding files. It
# controls the data pipeline and defines the global settings.

###################################################
# PIPELINE SETTINGS
###################################################

#--------------------------------------------------
# load libraries

suppressPackageStartupMessages({
    library(targets)
    library(renv)
    library(dplyr)
    library(here)
    library(tarchetypes)
    library(fst)
    library(docstring)
    library(crew)
    library(glue)
    library(cli)
    library(tidyr)
    library(openxlsx)
    library(gdata)
    library(qs)
    library(sf)
})

#--------------------------------------------------
# working directory

setwd(here())

#--------------------------------------------------
# Pipeline settings

# target options
tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 50)
    ),
    seed = 1,
    garbage_collection = TRUE,
    memory = "transient",
    controller = crew_controller_local(
        name = "my_controller",
        workers = 3,
        seconds_idle = 10
    ),
    retrieval = "worker",
    storage = "worker"
)

#--------------------------------------------------
# load configurations
# TODO NEW WAVE: Adjust the globals to new delivery
# TODO NEW WAVE: Check if list of variables to be removed is still correct

source(
    file.path(
        here::here(),
        "code",
        "helpers",
        "config.R"
    )
)

#--------------------------------------------------
# load R scripts

sub_directories <- list.dirs(
    config_paths()[["code_path"]],
    full.names = FALSE,
    recursive = FALSE
)

for (sub_directory in sub_directories) {
    if (sub_directory != "helpers") { 
        lapply(
            list.files(
                file.path(
                    config_paths()[["code_path"]],
                    sub_directory
                ),
                pattern = "\\.R$",
                full.names = TRUE,
                ignore.case = TRUE
            ),
            source
        )
    } else {
        files <- list.files(
            file.path(
                config_paths()[["code_path"]],
                sub_directory
            ),
            pattern = "\\.R$",
            full.names = TRUE,
            ignore.case = TRUE
        )
        files <- files[
            stringr::str_detect(
                files,
                "config.R$"
            ) == FALSE
        ]
        lapply(files, source)
    }
}

###################################################
# ACTUAL PIPELINE
###################################################

#--------------------------------------------------
# Folder generation

targets_preparation_folders <- rlang::list2(
    tar_target(
        empty_folders,
        creating_folder_structure()
    )
)

#--------------------------------------------------
# MAYBE DELETE LATER
# define column names from previous deliveries
# NOTE: even though this is define in config as global, I am loading it here
# as global to avoid reading the data set in the config specification over and
# over again. This saves runtime.

#--------------------------------------------------
# Prepare original files for further processing
# Adjust the file naming
# NOTE: This step is only if the new delivery comes in subfolders (add else clause
# if needed). The past deliveries already have been cleaned.

targets_files <- rlang::list2(
    tar_target(
        file_naming,
        making_consistent_file_naming(
            current_delivery = config_globals()[["current_delivery"]]
        )
    )
)

#--------------------------------------------------
# read original data

targets_reading <- rlang::list2(
    tar_file_read(
        org_data,
        file.path(
            config_paths()[["data_path"]],
            "original",
            config_globals()[["current_delivery"]],
            "commercial_data_all.csv"
        ),
        suppressWarnings(
            reading_org_data(
                !!.x
            )
        )
    ),
    tar_eval(
        list(
            tar_qs(
                spatial_data,
                reading_geo_data(
                    spatial_unit = spatial_unit_german,
                    year = 2019,
                    ags_name = ags_names,
                    gen_name = gen_names
                )
            )
        ),
        values = list(
            spatial_data = rlang::syms(helpers_target_names()[["spatial_data"]]), 
            spatial_unit_german = helpers_target_names()[["spatial_units_german"]],
            ags_names = helpers_target_names()[["ags_names"]],
            gen_names = helpers_target_names()[["gen_names"]]
        )
    ),
    tar_file_read(
        spatial_data_grids,
        file.path(
            config_paths()[["gebiete_path"]],
            "Raster",
            "grids_BRD.shp"
        ),
        reading_grids(!!.x)
    )
)

#--------------------------------------------------
# Preparation of the original data
# NOTE: This step is only needed for the new delivery since the past deliveries
# already have been cleaned.

targets_preparation <- rlang::list2(
    tar_fst(
        column_infos_benchmark,
        exporting_column_infos(
            housing_data = org_data
        )
    ),
    # NOTE: if the pipeline stops due to inconsistent variables, make the appropriate
    # changes and when done add the variable to the list of
    # fixed variables in config.R
    tar_target(
        test_consistent_variables,
        testing_consistent_variables(
            housing_data = org_data,
            column_infos_benchmark = column_infos_benchmark
        )
    ),
    tar_fst(
        housing_data_cleaned,
        # suppress warnings because conversion of characters to numeric
        # generates warnings
        suppressWarnings(
            cleaning_org_data(
                housing_data = org_data
            )
        )
    ),
    tar_target(
        housing_data_geo,
        georeferencing_housing_data(
            housing_data = housing_data_cleaned,
            spatial_data_grids = spatial_data_grids,
            spatial_data_zip_code = spatial_data_zip_code,
            spatial_data_municipality = spatial_data_municipality,
            spatial_data_district = spatial_data_district
        )
    ),
    tar_fst(
        finalized_data,
        testing_missing_variables(
            housing_data = housing_data_geo
        )
    ),
    tar_target(
        removed_variables,
        testing_removed_variables()
    )
)

#--------------------------------------------------
# combine each wave

targets_append <- rlang::list2(
    tar_fst(
        housing_data_appended,
        appending_waves(
            deliveries = config_globals()[["deliveries"]]
        )
    )
)

#--------------------------------------------------
# cleaning on combined data set

targets_combine_cleaning <- rlang::list2(
    tar_fst(
        housing_data_append_cleaned,
        clean_append_data(
            housing_data = housing_data_append
        )
    )
)

#--------------------------------------------------
# Unit testing
# TODO: entire block

targets_unit_testing <- rlang::list2(
    # check that all variables have the right type

    # check that all variables are in reasonable ranges
    # compare to previous delivery

    # check that municipality, district, state ID have the correct character
    # lengths (8, 5, 2)
)

#--------------------------------------------------
# pipeline stats

targets_pipeline_stats <- rlang::list2(
    # NOTE: targets type has to be tar_file in order to use tar_progress_summary
    # and tar_crew within the pipeline
    tar_file(
        pipeline_stats,
        helpers_monitoring_pipeline(),
        cue = tar_cue(mode = "always")
    )
)

#--------------------------------------------------
# combine all target branches

rlang::list2(
    targets_preparation_folders,
    targets_files,
    targets_reading,
    targets_preparation,
    targets_append,
    #t argets_combine_cleaning,
    targets_pipeline_stats
)