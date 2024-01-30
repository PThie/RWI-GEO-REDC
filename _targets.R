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
# Prepare original files for further processing
# Adjust the file naming

targets_files <- rlang::list2(
    tar_target(
        file_naming,
        make_consistent_file_naming(
            current_delivery = config_globals()[["current_delivery"]]
        )
    )
)

#----------------------------------------------
# Preparation of the original data

targets_preparation <- rlang::list2(
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
        # suppress warnings because conversion of characters to numeric
        # generates warnings
        suppressWarnings(
            read_org_data(
                !!.x
            )
        )
    ),
    tar_target(
        org_data_geo,
        georeferencing(
            org_data = org_data
        )
    )
)

#----------------------------------------------
# combine all target branches

rlang::list2(
    targets_files,
    #targets_preparation
)