reading_microm_data <- function() {
    #' @title Read the microm data
    #' 
    #' @description This function reads the microm data.
    #' 
    #' @return Dataframe with the microm data
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read data

    microm_data <- haven::read_dta(
        file.path(
            config_paths()[["microm_data_path"]],
            config_globals()[["microm_data_version"]],
            "microm_panel_05-22.dta"
        )
    )

    #--------------------------------------------------
    # return

    return(microm_data)
}