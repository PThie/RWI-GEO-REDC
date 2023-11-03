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

main_path <- here()
setwd(main_path)

#----------------------------------------------
# paths

output_path <- file.path(
    main_path,
    "output"
)

data_path <- file.path(
    main_path,
    "data"
)

code_path <- file.path(
    main_path,
    "code"
)

gebiete_path <- file.path(
    "M:/_FDZ/interne Daten/Gebietseinheit"
)

#----------------------------------------------
# Read main files

lapply(
    list.files(
        file.path(main_path, "code"),
        pattern = ".R$",
        full.names = TRUE,
        all.files = FALSE
    ),
    source
)

#----------------------------------------------
# globals

# define current delivery
current_delivery <- "Lieferung_2306"

# current version of REDC
current_version <- "v1"

# maximum year in the current delivery
max_year <- 2023

#----------------------------------------------
# folder generation for new delivery (in data folder)

for (data_folder in c("on-site", "processed", "SUF")) {
    ifelse(
        !dir.exists(
            file.path(
                data_path,
                data_folder,
                current_version
            )
        ),
        yes = dir.create(
            file.path(
                data_path,
                data_folder,
                current_version
            )
        ),
        no = cli::cli_alert_success(
            col_green(
                "Version directory for \"{data_folder}\" data folder already exists."    
            )
        )
    )
}

#----------------------------------------------

targets_preparation <- rlang::list2(
    tar_file_read(
        org_data,
        # path to original data (automatically paste into read sfunction)
        file.path(
            data_path,
            "original",
            current_delivery,
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
        georeferencing(org_data = org_data)
    )
)

#----------------------------------------------
# combine all target branches

rlang::list2(
    targets_preparation
)