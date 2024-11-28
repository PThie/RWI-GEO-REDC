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
# NOTE: This step is only needed for the new delivery since the past deliveries
# already have been cleaned.
# TODO-NEW-DELIVERY: Adjust the file naming in else clause

targets_files <- rlang::list2(
    tar_target(
        file_naming,
        make_consistent_file_naming(
            current_delivery = config_globals()[["current_delivery"]]
        )
    )
)

#--------------------------------------------------
# read original data

targets_reading <- rlang::list2(
    tar_file_read(
        org_data,
        # path to original data (automatically paste into read sfunction)
        file.path(
            config_paths()[["data_path"]],
            "original",
            config_globals()[["current_delivery"]],
            "commercial_data_all.csv"
        ),
        # actual reading
        suppressWarnings(
            read_org_data(
                !!.x
            )
        )
    )
)

#--------------------------------------------------
# Preparation of the original data
# NOTE: This step is only needed for the new delivery since the past deliveries
# already have been cleaned.

targets_preparation <- rlang::list2(
    # NOTE: if the pipeline stops due to inconsistent variables, make the appropriate
    # changes in read_org_data.R and when done add the variable to the list of
    # fixed variables in testing_consistent_variables.R
    tar_target(
        org_data_expanded,
        testing_consistent_variables(
            org_data = org_data,
            current_delivery = config_globals()[["current_delivery"]]
        )
    ),
    tar_fst(
        org_data_cleaned,
        # suppress warnings because conversion of characters to numeric
        # generates warnings
        suppressWarnings(
            clean_org_data(
                org_data_expanded = org_data_expanded,
                current_delivery = config_globals()[["current_delivery"]],
                max_year = config_globals()[["max_year"]]
            )
        )
    ),
    tar_target(
        org_data_geo,
        georeferencing(
            org_data_cleaned = org_data_cleaned
        )
    )
)

#--------------------------------------------------
# combine each wave

targets_append <- rlang::list2(
    tar_target(
        org_data_append,
        append_waves(
            deliveries = config_globals()[["deliveries"]]
        )
    )
)

#--------------------------------------------------
# cleaning on combined data set

targets_combine_cleaning <- rlang::list2(
    tar_fst(
        org_data_append_cleaned,
        clean_append_data(
            org_data_append = org_data_append
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
# combine all target branches

rlang::list2(
    targets_preparation_folders,
    # targets_files,
    # targets_reading,
    # targets_preparation,
    # targets_append,
    #t argets_combine_cleaning
)