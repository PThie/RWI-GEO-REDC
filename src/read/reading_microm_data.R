reading_microm_data <- function() {
    #' @title Read the microm data
    #' 
    #' @description This function reads the microm data.
    #' 
    #' @return Dataframe with the microm data
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    microm_data <- data.table::fread(
        file.path(
            config_paths()[["microm_data_path"]],
            config_globals()[["microm_data_version"]],
            paste0(
                config_globals()[["microm_file_name"]],
                ".csv"
            )
        )
    )

    #--------------------------------------------------
    # return

    return(microm_data)
}