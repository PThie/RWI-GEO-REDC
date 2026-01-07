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
    library(data.table)
    library(stringr)
    library(arrow)
    library(zoo)
    library(ggplot2)
    library(MetBrewer)
    library(autometric)
    library(kableExtra)
})

#--------------------------------------------------
# working directory

setwd(here())

#--------------------------------------------------
# load configurations
# TODO NEW WAVE: Adjust the globals to new delivery
# TODO NEW WAVE: Check if list of variables to be removed is still correct

source(
    file.path(
        here::here(),
        "src",
        "helpers",
        "config.R"
    )
)

#--------------------------------------------------
# Pipeline settings

# target options
controller <- crew_controller_local(
    name = "worker",
    workers = 3,
    seconds_idle = 10,
    options_metrics = crew_options_metrics(
        path = file.path(
            config_paths()[["logs_path"]],
            "worker_metrics",
            "worker_metrics_history"
        ),
        seconds_interval = 1
    )
)

tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 50)
    ),
    seed = 1,
    garbage_collection = TRUE,
    memory = "transient",
    controller = controller,
    retrieval = "worker",
    storage = "worker"
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
# Prepare original files for further processing
# Adjust the file naming
# NOTE: This step is only needed if the new delivery comes in subfolders (add else clause
# if needed). The past deliveries already have been cleaned.
# NOTE: Currently, only affects Lieferung_2312.

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
    #--------------------------------------------------
    # housing data
    tar_file_read(
        housing_data_org,
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
    #--------------------------------------------------
    # microm (GRID) data
    tar_fst(
        microm_data,
        reading_microm_data()
    ),
    #--------------------------------------------------
    # spatial data
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
    #--------------------------------------------------
    # preparing microm (GRID) data
    tar_fst(
        microm_data_cleaned,
        cleaning_microm_data(
            microm_data = microm_data
        )
    ),
    #--------------------------------------------------
    # preparing housing data
    tar_fst(
        column_infos_benchmark,
        exporting_column_infos(
            housing_data = housing_data_org
        )
    ),
    # NOTE: if the pipeline stops due to inconsistent variables, make the appropriate
    # changes and when done add the variable to the list of
    # fixed variables in config.R
    tar_target(
        test_consistent_variables,
        testing_consistent_variables(
            housing_data = housing_data_org,
            column_infos_benchmark = column_infos_benchmark
        )
    ),
    tar_fst(
        housing_data_cleaned,
        # suppress warnings because conversion of characters to numeric
        # generates warnings
        suppressWarnings(
            cleaning_org_data(
                housing_data = housing_data_org
            )
        )
    ),
    #--------------------------------------------------
    # georeferencing housing data
    # NOTE: properties with and without coordinates are treated separately
    tar_target(
        housing_data_coordinates,
        adding_conventional_coordinates(
            housing_data = housing_data_cleaned
        )
    ),
    # joining other spatial units
    tar_fst(
        housing_data_district,
        adding_spatial_units(
            housing_data = housing_data_coordinates[["housing_data_coords"]],
            spatial_data = spatial_data_district
        )
    ),
    tar_fst(
        housing_data_municipality,
        adding_spatial_units(
            housing_data = housing_data_district,
            spatial_data = spatial_data_municipality
        )
    ),
    tar_fst(
        housing_data_zip_code,
        adding_spatial_units(
            housing_data = housing_data_municipality,
            spatial_data = spatial_data_zip_code
        )
    ),
    tar_fst(
        housing_data_lmr,
        adding_spatial_units(
            housing_data = housing_data_zip_code,
            spatial_data = spatial_data_lmr
        )
    ),
    tar_fst(
        housing_data_grids,
        adding_spatial_units(
            housing_data = housing_data_lmr,
            spatial_data = spatial_data_grids
        )
    ),
    tar_fst(
        housing_data_spatial_cleaned,
        cleaning_housing_spatial_data(
            housing_data = housing_data_grids
        )
    ),
    # cleaning housing data without coordinates
    tar_fst(
        housing_data_spatial_wo_coordinates_cleaned,
        cleaning_housing_spatial_without_coordinates(
            housing_data = housing_data_coordinates[["housing_data_wo_coords"]],
            spatial_data_district = spatial_data_district,
            spatial_data_municipality = spatial_data_municipality
        )
    ),
    # # combine both datasets again
    tar_fst(
        housing_data_coordinates_combined,
        appending_housing_with_without_coordinates(
            housing_data_with_coordinates = housing_data_spatial_cleaned,
            housing_data_without_coordinates = housing_data_spatial_wo_coordinates_cleaned
        )
    ),
    #--------------------------------------------------
    # finalizing housing data
    tar_fst(
        finalized_data,
        testing_missing_variables(
            housing_data = housing_data_coordinates_combined
        )
    ),
    tar_target(
        removed_variables,
        testing_removed_variables(
            dependency = finalized_data
        )
    ),
    #--------------------------------------------------
    # fix for bef variables in delivery 2306
    tar_fst(
        housing_data_fixed_bef_2306,
        fixing_bef_del_2306()
    ),
    #--------------------------------------------------
    tar_fst(
        housing_data_fixed_lmr_2306,
        fixing_lmr_del_2306(
            lmr_data = spatial_data_lmr
        )
    )
)

#--------------------------------------------------
# combine each wave

targets_append <- rlang::list2(
    tar_fst(
        housing_data_appended,
        appending_waves(
            deliveries = config_globals()[["deliveries"]],
            dependencies = list(
                finalized_data,
                housing_data_fixed_lmr_2306
            )
        )
    )
)

#--------------------------------------------------
# cleaning on combined data set

targets_combine_cleaning <- rlang::list2(
    tar_fst(
        housing_data_append_cleaned,
        cleaning_append_data(
            housing_data = housing_data_appended
        )
    ),
    tar_fst(
        housing_data_fixed_new,
        cleaning_missings_new_variables(
            housing_data = housing_data_append_cleaned
        )
    ),
    tar_fst(
        housing_data_fixed_missings,
        fixing_remaining_missings(
            housing_data = housing_data_fixed_new
        )
    ),
    tar_fst(
        housing_data_translated,
        translating_variables(
            housing_data = housing_data_fixed_missings
        )
    )
)

#--------------------------------------------------
# SUF cleaning

targets_suf_cleaning <- rlang::list2(
    tar_fst(
        housing_data_suf_anonymized,
        anonymizing_SUF_data(
            housing_data = housing_data_translated,
            microm_data = microm_data_cleaned
        )
    ),
    tar_fst(
        housing_data_suf_cleaned,
        cleaning_SUF_data(
            housing_data = housing_data_suf_anonymized
        )
    )
)

#--------------------------------------------------
# Documentation

targets_documentation <- rlang::list2(
    tar_fst(
        variable_labels,
        creating_variable_labels(
            housing_data = housing_data_translated
        )
    ),
    tar_fst(
        value_labels,
        creating_value_labels(
            housing_data = housing_data_translated,
            variable_labels = variable_labels
        )
    ),
    tar_eval(
        list(
            tar_fst(
                spatial_unit_names,
                exporting_spatial_unit_names(
                    spatial_units = spatial_data
                )
            )
        ),
        values = list(
            spatial_unit_names = rlang::syms(helpers_target_names()[["spatial_unit_names"]][1:2]),
            spatial_data = rlang::syms(helpers_target_names()[["spatial_data"]][1:2])
        )
    ),
    tar_fst(
        dataset_info,
        exporting_dataset_info(
            housing_data = housing_data_translated
        )
    )
)

#--------------------------------------------------
# Export

targets_export <- rlang::list2(
    tar_target(
        housing_data_exported,
        exporting_housing_data(
            onsite_data = housing_data_translated,
            suf_data = housing_data_suf_cleaned
        )
    )
)

#--------------------------------------------------
# Unit testing

targets_unit_testing <- rlang::list2(
    tar_eval(
        list(
            #--------------------------------------------------
            # Reading the exported SUF data
            tar_file_read(
                suf_exported_data,
                file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["current_version"]],
                    exported_file_formats,
                    paste0(
                        "REDC_",
                        config_globals()[["current_version"]],
                        "_SUF.",
                        exported_file_formats
                    )
                ),
                reading_exported_data(
                    data_path = !!.x,
                    file_format = exported_file_formats,
                    dependencies = list(
                        housing_data_org,
                        housing_data_exported
                    )
                )
            ),
            #--------------------------------------------------
            # Test whether all variables have been deleted that are sensitive
            tar_target(
                suf_compliance_test,
                testing_SUF_compliance(
                    suf_data = suf_exported_data,
                    file_format = exported_file_formats
                )
            ),
            #--------------------------------------------------
            # Test whether anonymization has been done correctly
            tar_target(
                suf_anonymization_test,
                testing_SUF_anonymization(
                    suf_data = suf_exported_data,
                    microm_data = microm_data_cleaned
                )
            ),
            #--------------------------------------------------
            # Test whether all NAs have been recoded
            tar_fst(
                missings_recoding_test,
                testing_missings_recoding(
                    suf_data = suf_exported_data,
                    file_format = exported_file_formats
                )
            )
        ),
        values = list(
            suf_exported_data = rlang::syms(helpers_target_names()[["suf_exported_data"]]),
            exported_file_formats = helpers_target_names()[["exported_file_formats"]],
            suf_compliance_test = rlang::syms(helpers_target_names()[["suf_compliance_test"]]),
            suf_anonymization_test = rlang::syms(helpers_target_names()[["suf_anonymization_test"]]),
            missings_recoding_test = rlang::syms(helpers_target_names()[["missings_recoding_test"]])
        )
    ),
    tar_target(
        temporal_trends_test,
        testing_temporal_trends(
            housing_data = housing_data_translated
        )
    ),
    tar_target(
        summary_stats_delivery,
        testing_summary_stats(
            housing_data = housing_data_translated,
            variable_labels = variable_labels
        )
    ),
    tar_target(
        spatial_trends_test,
        testing_spatial_trends(
            housing_data = housing_data_translated,
            districts = spatial_data_district
        )
    )
)

#--------------------------------------------------
# pipeline stats

targets_pipeline_stats <- rlang::list2(
	tar_file(
		pipeline_stats,
		helpers_monitoring_pipeline(),
		cue = tar_cue(mode = "always")
	),
    tar_target(
        worker_stats,
        reading_worker_stats(),
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
    targets_combine_cleaning,
    targets_suf_cleaning,
    targets_documentation,
    targets_export,
    targets_unit_testing,
    targets_special_requests,
    targets_pipeline_stats
)