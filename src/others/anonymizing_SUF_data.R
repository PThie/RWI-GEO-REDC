anonymizing_SUF_data <- function(
    housing_data = NA,
    microm_data = NA
) {
    #' @title Anonymizing SUF data
    #' 
    #' @description This function anonymizes the SUF data by censoring the grid ID
    #' if the threshold for number of businesses is undercut.
    #' 
    #' @param housing_data Dataframe with housing data
    #' @param microm_data Dataframe with microm data
    #' 
    #' @return Dataframe with anonymized SUF data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # merge housing data and microm (GRID) data

    merged_data <- merge(
        housing_data,
        microm_data,
        by.x = c("ergg_1km", "ejahr"),
        by.y = c("ergg_1km", "year"),
        all.x = TRUE
    )

    #--------------------------------------------------
    # censor grids if threshold is undercut

    # calculate number of missings before censoring
    num_missing_before <- length(which(
        merged_data$ergg_1km < 0
    ))

    # censore grid ID
    merged_data <- merged_data |>
        dplyr::mutate(
            ergg_1km = dplyr::case_when(
                censor_grid_id == 1 ~ as.character(
                    helpers_missing_values()[["other"]]
                ),
                TRUE ~ ergg_1km
            )
        ) |>
        dplyr::select(
            -censor_grid_id
        )

    # calculate number of missings after censoring
    num_missing_after <- length(which(
        merged_data$ergg_1km < 0
    ))

    # report number of missings before and after censoring
    missings <- as.data.frame(rbind(
        total_NOBS = nrow(merged_data),
        missing_before_censoring = num_missing_before,
        missing_before_censoring_perc = (num_missing_before / nrow(merged_data)) * 100,
        missing_after_censoring = num_missing_after,
        missing_after_censoring_perc = (num_missing_after / nrow(merged_data)) * 100
    )) |>
        dplyr::rename(NOBS = 1) |>
        dplyr::mutate(
            NOBS = round(NOBS, 2)
        )
    
    missings$var <- rownames(missings)
    rownames(missings) <- NULL
    missings <- missings |>
        dplyr::relocate(var)

    # export for documentation
    gdata::write.fwf(
        missings,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["current_version"]],
            "info",
            "missings_grid_id.txt"
        ),
        rownames = FALSE,
        colnames = FALSE
    )

    #--------------------------------------------------
    # return

    return(merged_data)
}