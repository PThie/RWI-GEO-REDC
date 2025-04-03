exporting_column_infos <- function(
    housing_data = NA
) {
    #' @title Exporting column infos of first delivery
    #' 
    #' @description This function exports the column names and types of the
    #' first delivery as benchmark to check for consistency in the following
    #' deliveries.
    #' 
    #' @param housing_data Dataframe with original housing data
    #' 
    #' @return Dataframe with column names and types
    #' @author Patrick Thiel 

    #--------------------------------------------------
    # export column names and types only for the very first delivery
    # (= benchmark)

    if (config_globals()[["current_delivery"]] == "Lieferung_2306") {
        #--------------------------------------------------
        # extract column types and names

        coltypes <- helpers_extracting_column_info(
            housing_data = housing_data
        )

        #--------------------------------------------------
        # export
        # NOTE: version is hard coded because the version in globals changes

        data.table::fwrite(
            coltypes,
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "column_types.csv"
            )
        )
    } else {
        #--------------------------------------------------
        # re-read column types and names of the first delivery
        # NOTE: version is hard coded because the version in globals changes
        
        coltypes <- data.table::fread(
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "column_types.csv"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(coltypes)
}
