fixing_lmr_del_2306 <- function(
    lmr_data = NA 
) {
    #' @title Fixing lmr data for delivery 2306
    #' 
    #' @description This function fixes the issue that the LMR information was
    #' missing in the originally cleaned data for delivery 2306 (delivery 1).
    #' This function adds the LMR information to the data set by using the
    #' same strategy as for the other deliveries/ spatial units.
    #'
    #' @param lmr_data Dataframe with LMR information.
    #' 
    #' @return Dataframe with fixed LMR information.
    #' @author Patrick Thiel

    #--------------------------------------------------

    # NOTE: set specific to delivery 23121 even though this applies to all the
    # versions of RWI-GEO-REDC in the future as well. But setting it here to 
    # 23121 means that the calculation does not rerun each time saving time.
    if (config_globals()[["current_delivery"]] == "Lieferung_23121") {
        #--------------------------------------------------
        # read processed data of delivery 2306 (as published as V1)
        # NOTE: you have to use clean_data_fixed_bef.parquet because the previous
        # fix fixes the bef variables in that delivery.
        processed_data <- arrow::read_parquet(
            file.path(
                config_paths()[["data_path"]],
                "processed",
                "Lieferung_2306",
                "fixed_bef",
                "clean_data_fixed_bef.parquet"
            )
        )

        #--------------------------------------------------
        # split into data with and without coordinates

        with_coords <- processed_data |>
            dplyr::filter(lon_gps > 0)

        without_coords <- processed_data |>
            dplyr::filter(lon_gps < 0)

        #--------------------------------------------------
        # add spatial units to data with coordinates

        with_coords_lmr <- adding_spatial_units(
            housing_data = with_coords,
            spatial_data = lmr_data
        )

        #--------------------------------------------------
        # add all missings for data without coordinates

        without_coords_lmr <- without_coords |>
            dplyr::mutate(
                lmr2018 = as.character(helpers_missing_values()[["other"]])
            )

        #--------------------------------------------------
        # combine data with and without coordinates

        processed_data_prep <- rbind(
            with_coords_lmr,
            without_coords_lmr
        )

        #--------------------------------------------------
        # export

        arrow::write_parquet(
            processed_data_prep,
            file.path(
                config_paths()[["data_path"]],
                "processed",
                "Lieferung_2306",
                "fixed_lmr",
                "clean_data_fixed_lmr.parquet"
            )
        )
    } else {
        # read previously fixed LMR data from parquet file
        processed_data_prep <- arrow::read_parquet(
            file.path(
                config_paths()[["data_path"]],
                "processed",
                "Lieferung_2306",
                "fixed_lmr",
                "clean_data_fixed_lmr.parquet"
            )
        )
    }
    

    #--------------------------------------------------
    # return
    return(processed_data_prep)
}

