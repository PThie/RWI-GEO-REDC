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

suppressPackageStartupMessages({
    # used during setup of pipeline
    library(targets)
    library(tarchetypes)
    library(future)
    library(future.callr)
    library(fst)
    library(renv)
    library(rlang)
    library(qs)
    library(docstring)

    # used during execution of pipeline
    library(stringr)
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

main_path <- "M:/_FDZ/RWI-GEO/RWI-GEO-REDC/"
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

#----------------------------------------------

targets_preparation <- rlang::list2(
    tar_target(
        reading_org_data,
        read_org_data()
    )
)

#----------------------------------------------
# combine all target branches

rlang::list2(

)