creating_folder_structure <- function() {
    #' @title Create folder structure
    #' 
    #' @description This function creates the folder structure for the output
    #' of a new wave.
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #----------------------------------------------
    # folder generation for new delivery (in data folder)

    for (data_folder in config_globals()[["data_folders"]]) {
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
                    cli::col_green(
                        "Version directory for \"{data_folder}\" data folder already exists."    
                    )
                )
            )
        }
    }
  
    #--------------------------------------------------
    # create version folder in output
  
    ifelse(
        !dir.exists(
            file.path(
                config_globals()[["output_path"]],
                config_globals()[["current_version"]]
            )
        ),
        yes = dir.create(
            file.path(
                config_globals()[["output_path"]],
                config_globals()[["current_version"]]
            )
        ),
        no = cli::cli_alert_success(
            cli::col_green(
                "Version directory for output folder already exists."
            )
        )
    )

    #--------------------------------------------------
    # return

    return(NULL)
}