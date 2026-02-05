reading_org_data <- function(redc_raw_file = NA) {
    #' @title Reading original data
    #' 
    #' @description This function reads the raw data.
    #' 
    #' @param redc_raw_file File path to raw/ original data
    #' 
    #' @note Separating the reading process from the cleaning process avoids
    #' that the original data has to be reloaded multiple times.
    #' 
    #' @return DataFrame
    #' @author Patrick Thiel

    #--------------------------------------------------
    # special handling for Delivery 2601
    # NOTE: Delivery 2601 contains only the two missing years 2020 and 2021

    if (config_globals()[["current_delivery"]] == "Lieferung_2601") {
        #--------------------------------------------------
        # files

        rwi_com_files <- list.files(
            path = file.path(
                config_paths()[["data_path"]],
                "original",
                "Lieferung_2601",
                "OneDrive_1_8.1.2026"
            ),
            pattern = "RWI_COM_.*\\.csv$",
            full.names = TRUE
        )

        #--------------------------------------------------
        # read helper
        
        read_file_dt <- function(file_path) {
            dt <- data.table::fread(file_path, encoding = "UTF-8")
            data.table::setnames(dt, tolower(names(dt)))
            return(dt)
        }

        dt_list <- lapply(rwi_com_files, read_file_dt)
    
        # check for difference in colum names between both years
        col_diff <- base::setdiff(
            colnames(dt_list[[1]]),
            colnames(dt_list[[2]])
        )

        targets::tar_assert_true(
            length(col_diff) == 0,
            msg = glue::glue(
                "!!! WARNING:",
                " Column names differ between 2020 and 2021.",
                " (Error code: rod#1)"
            )
        )

        #--------------------------------------------------
        # combine both datasets

        org_data <- data.table::rbindlist(
            dt_list
        )
    } else {
        #----------------------------------------------
        # read data
    
        org_data <- data.table::fread(
            redc_raw_file,
            encoding = "UTF-8"
        )
    }

    #--------------------------------------------------
    # standardize the primary dataset column names to lowercase before merging

    data.table::setnames(org_data, tolower(names(org_data)))   

    #--------------------------------------------------
    # check that dataframe is not empty

    targets::tar_assert_nonempty(
        org_data,
        msg = glue::glue(
            "!!! WARNING:",
            " Dataset is empty.",
            " (Error code: rod#2)"
        )
    )

    #--------------------------------------------------
    # return

    return(org_data)
}