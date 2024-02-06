#----------------------------------------------
# used during execution of pipeline

pipeline_library <- c(
    "stringr", # text manipulation via regex
    "dplyr", # data manipulation
    "data.table", # data manipulation
    "fst", # data file storage format
    "sf", # geo-data manipulation
    "ggplot2", # plotting
    "MetBrewer" # colors
)

# install.packages(
#     c(
#         "targets", "tarchetypes", "future", "future.callr",
#         "fst", "renv", "rlang", "qs", "docstring", "cli",
#         "stringr", "stringi", "dplyr", "tidyr", "data.table",
#         "sf", "openxlsx", "ggplot2", "MetBrewer", "here"
#     )
# )

suppressPackageStartupMessages({
    # used during setup of pipeline
    library(here)
    library(targets)
    library(tarchetypes)
    library(future)
    library(future.callr)
    library(fst)
    library(renv)
    library(rlang)
    library(qs)
    library(docstring)
    library(cli)

    # used during execution of pipeline
    library(stringr)
    library(stringi)
    library(dplyr)
    library(tidyr)
    library(data.table)
    library(sf)
    library(openxlsx)
    library(ggplot2)
    library(MetBrewer)
})

#----------------------------------------------
# set up for future package

tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 100)
    ),
    packages = pipeline_library,
    seed = 1,
    garbage_collection = TRUE,
    memory = "transient"
)

#----------------------------------------------
# working directory

setwd(here())

#----------------------------------------------
# load configurations
# TODO-NEW-DELIVERY: Adjust the globals to new delivery
# TODO-NEW-DELIVERY: Check if list of variables to be removed is still correct

source(
    file.path(
        here(),
        "code",
        "helpers",
        "config.R"
    )
)

#----------------------------------------------
# Read main files

lapply(
    list.files(
        config_paths()[["code_path"]],
        pattern = ".R$",
        full.names = TRUE,
        all.files = FALSE
    ),
    source
)

#----------------------------------------------
# folder generation for new delivery (in data folder)

for (data_folder in c("on-site", "processed", "SUF")) {
    if (data_folder == "processed") {
        ifelse(
            !dir.exists(
                file.path(
                    config_paths()[["data_path"]],
                    data_folder,
                    config_globals()[["current_delivery"]]
                )
            ),
            yes = dir.create(
                file.path(
                    config_paths()[["data_path"]],
                    data_folder,
                    config_globals()[["current_delivery"]]
                )
            ),
            no = cli::cli_alert_success(
                col_green(
                    "Delivery directory for \"{data_folder}\" data folder already exists."    
                )
            )
        )
    } else {
        ifelse(
            !dir.exists(
                file.path(
                    config_paths()[["data_path"]],
                    data_folder,
                    config_globals()[["current_version"]]
                )
            ),
            yes = dir.create(
                file.path(
                    config_paths()[["data_path"]],
                    data_folder,
                    config_globals()[["current_version"]]
                )
            ),
            no = cli::cli_alert_success(
                col_green(
                    "Version directory for \"{data_folder}\" data folder already exists."    
                )
            )
        )
    }
}

#----------------------------------------------
# MAYBE DELETE LATER
# define column names from previous deliveries
# NOTE: even though this is define in config as global, I am loading it here
# as global to avoid reading the data set in the config specification over and
# over again. This saves runtime.

#----------------------------------------------
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

#----------------------------------------------
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

#----------------------------------------------
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
    # tar_target(
    #     org_data_geo,
    #     georeferencing(
    #         org_data_cleaned = org_data_cleaned
    #     )
    # )
)

#----------------------------------------------
# Unit testing
# TODO: entire block

targets_unit_testing <- rlang::list2(
    # check that all variables have the right type

    # check that all variables are in reasonable ranges
    # compare to previous delivery
)

#----------------------------------------------
# combine all target branches

rlang::list2(
    # targets_files,
    targets_reading,
    targets_preparation
)