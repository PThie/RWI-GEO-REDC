make_consistent_file_naming <- function(current_delivery = NA) {
    #' @title Make consistent file naming
    #' 
    #' @description This function guarantees consistent file naming for the next
    #' steps.
    #' 
    #' @param current_delivery The current delivery.
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # special handling of delivery 2312 since it includes all past data (2010 to
    # 2022)

    if (current_delivery == "Lieferung_2312") {
        # list all folders
        file_list <- list.files(
            file.path(
                config_paths()[["data_path"]],
                "original",
                current_delivery
            ),
            pattern = "*.csv$"
        )

        # data storage
        data_storage <- list()

        # read all files
        for (file in file_list) {
            dta <- data.table::fread(
                file.path(
                    config_paths()[["data_path"]],
                    "original",
                    current_delivery,
                    file,
                    file
                )
            )

            # set date as character
            dta$Einstelldatum <- as.character(dta$Einstelldatum)

            data_storage[[file]] <- dta
        }
    
        # combine all files
        all_data <- data.table::rbindlist(
            data_storage,
            fill = TRUE
        )

        # export
        data.table::fwrite(
            all_data,
            file.path(
                config_paths()[["data_path"]],
                "original",
                current_delivery,
                "commercial_data_all.csv"
            )
        )
    } else {
        # TODO-NEW-DELIVERY: Add here code for new delivery depending how the
        # original files are named
    }

    #----------------------------------------------
    # return

    return(NULL)
}