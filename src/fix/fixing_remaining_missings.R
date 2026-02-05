fixing_remaining_missings <- function(
    housing_data = NA
) {
    #' @title Fixing remaining missing values in the housing data
    #' 
    #' @description This function fixes the remaining missing values in the
    #' housing data and recodes the missing values to appropriate values.
    #' 
    #' @param housing_data Dataframe with housing data
    #' 
    #' @return Dataframe with fixed missing values
    #' @author Patrick Thiel

    #--------------------------------------------------
    # handle delivery specific missing values
    # TODO: move to config
    if (config_globals()[["current_delivery"]] %in% c(
        "Lieferung_23121", "Lieferung_2406",
        "Lieferung_2507", "Lieferung_2601"
    )) {
        #--------------------------------------------------
        # handle variables that are not always present
        # NOTE: these variables are introduced in the second delivery (delivery 2312)
        # however are not present in the first delivery (delivery 2306)
        # NOTE: some of them are still present in third delivery (delivery 23121)
        # like the performance variables (hits, click_*, liste_*)

        vars_unique_to_del_2312 <- c(
            "heizkosten",
            "heizkosten_in_wm_enthalten",
            "ev_wwenthalten",
            "hits",
            "click_schnellkontakte",
            "liste_show",
            "liste_match",
            "click_weitersagen",
            "click_url",
            "bef24",
            "bef25",
            "bef26"
        )

        for (var in vars_unique_to_del_2312) {
            # replace with appropriate missing value if completely missing
            if (typeof(housing_data[[var]]) == "character") {
                housing_data <- housing_data |>
                    dplyr::group_by(redc_delivery) |>
                    dplyr::mutate(
                        !!rlang::sym(var) := dplyr::case_when(
                            # Case 1: Entire year is NA → replace all NA if not available
                            all(is.na(!!rlang::sym(var))) ~ as.character(
                                helpers_missing_values()[["not_used_anymore"]]
                            ),
                            # Case 2: Only some NA → replace those NA with not_specified
                            is.na(!!rlang::sym(var)) ~ as.character(
                                helpers_missing_values()[["not_specified"]]
                            ),
                            TRUE ~ !!rlang::sym(var)
                        )
                    )
            } else {
                housing_data <- housing_data |>
                    dplyr::group_by(redc_delivery) |>
                    dplyr::mutate(
                        !!rlang::sym(var) := dplyr::case_when(
                            all(is.na(!!rlang::sym(var))) ~ helpers_missing_values()[["not_used_anymore"]],
                            is.na(!!rlang::sym(var)) ~ helpers_missing_values()[["not_specified"]],
                            TRUE ~ !!rlang::sym(var)
                        )
                    )|>
                dplyr::ungroup()
            }
        }
    }

    #--------------------------------------------------
    # handle missing in other variables that have not been recoded yet

    vars_to_recode <- c(
        "kname2019",
        "gname2019",
        "lmr2018"
    )

    for (var in vars_to_recode) {
        if (typeof(housing_data[[var]]) == "character") {
            housing_data <- housing_data |>
                dplyr::mutate(
                    !!rlang::sym(var) := dplyr::case_when(
                        is.na(!!rlang::sym(var)) ~ as.character(
                            helpers_missing_values()[["other"]]
                        ),
                        TRUE ~ !!rlang::sym(var)
                    )
                )
        } else {
            housing_data <- housing_data |>
                dplyr::mutate(
                    !!rlang::sym(var) := dplyr::case_when(
                        is.na(!!rlang::sym(var)) ~ helpers_missing_values()[["other"]],
                        TRUE ~ !!rlang::sym(var)
                    )
                )
        }
    }

    #--------------------------------------------------
    # stop the pipeline if there are still missing values that are not recoded

    # calculate number of missings per variable
    still_missings <- sapply(
        housing_data,
        function(x) {
            sum(is.na(x))
        }
    ) |>
    as.data.frame() |>
    dplyr::rename(
        num_missings = 1
    ) |>
    dplyr::filter(num_missings > 0)

    # reset column names
    still_missings$vars <- rownames(still_missings)
    rownames(still_missings) <- NULL
    
    # check if there are still missings
    targets::tar_assert_true(
        nrow(still_missings) == 0,
        msg = glue::glue(
            "!!! WARNING: ",
            "There are still missing values in the data that are not recoded. ",
            "Check the following variables: ",
            "{paste(still_missings$vars, collapse = ', ')}",
            " (Error code: frm#2)"
        )
    )

    #--------------------------------------------------
    # return

    return(housing_data)
}