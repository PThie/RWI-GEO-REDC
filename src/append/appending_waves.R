appending_waves <- function(
    deliveries = NA,
    dependency = NA
) {
    #' @title Combining deliveries
    #' 
    #' @description This function combines all deliveries into one data set.
    #' 
    #' @param deliveries List with all deliveries
    #' @param dependency Object from previous step to indicate dependency between
    #' functions/ steps
    #' 
    #' @return Dataframe, combined data set
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # read all data

    data_storage <- list()
    for (del in deliveries) {
        if (del == 2306) {
            dta <- arrow::read_parquet(
                file.path(
                    config_paths()[["data_path"]],
                    "processed",
                    paste0("Lieferung_", del),
                    "fixed_bef",
                    "clean_data_fixed_bef.parquet"
                )
            )
        } else {
            dta <- arrow::read_parquet(
                file.path(
                    config_paths()[["data_path"]],
                    "processed",
                    paste0("Lieferung_", del),
                    "clean_data.parquet"
                )
            )
        }

        data_storage[[del]] <- dta
    }

    #--------------------------------------------------
    # append all data

    dta_append <- data.table::rbindlist(data_storage, fill = TRUE)

    #--------------------------------------------------
    # return

    return(dta_append)
}