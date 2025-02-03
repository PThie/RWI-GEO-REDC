exporting_housing_data <- function(
    onsite_data = NA,
    suf_data = NA
) {
    #' @title Exporting final datasets
    #' 
    #' @description This function exports the final datasets to parquet and csv
    #' formats.
    #' 
    #' @param onsite_data Dataframe containing the on-site data, i.e. data for
    #' the secure room.
    #' @param suf_data Dataframe containing the Suf data, i.e. data that can
    #' leave the institute.
    #' 
    #' @return List with exported datasets.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # combine both datasets into list

    data <- list(
        "onsite" = onsite_data,
        "suf" = suf_data
    )

    #--------------------------------------------------
    # define names of exported dataset

    onsite_name <- paste0("REDC_", config_globals()[["current_version"]])
    suf_name <- paste0("REDC_", config_globals()[["current_version"]], "_SUF")

    #--------------------------------------------------
    # export each dataset

    for (dta_type in names(data)) {

        #--------------------------------------------------
        # export parquet

        if (dta_type == "onsite") {
            arrow::write_parquet(
                data[[dta_type]],
                file.path(
                    config_paths()[["data_path"]],
                    "on-site",
                    config_globals()[["current_version"]],
                    "parquet",
                    paste0(onsite_name, ".parquet")
                )
            )
        } else {
            arrow::write_parquet(
                data[[dta_type]],
                file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["current_version"]],
                    "parquet",
                    paste0(suf_name, ".parquet")
                )
            )
        }

        #--------------------------------------------------
        # export csv

        if (dta_type == "onsite") {
            data.table::fwrite(
                data[[dta_type]],
                file = file.path(
                    config_paths()[["data_path"]],
                    "on-site",
                    config_globals()[["current_version"]],
                    "csv",
                    paste0(onsite_name, ".csv")
                ),
                sep = ";",
                row.names = FALSE
            )
        } else {
            data.table::fwrite(
                data[[dta_type]],
                file = file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["current_version"]],
                    "csv",
                    paste0(suf_name, ".csv")
                ),
                sep = ";",
                row.names = FALSE
            )
        }
    }

    #--------------------------------------------------
    # return

    return(data)
}