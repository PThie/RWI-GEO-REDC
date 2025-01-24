cleaning_append_data <- function(
    housing_data = NA
) {
    #' @title
    #' 
    #' @description
    #' 
    #' @param housing_data
    #' 
    #' @return
    #' @author Patrick Thiel
    #' 
    #' @note on performance variables: They have only missing values. So apparently
    #' they are not reported for commercial listings.

    #--------------------------------------------------
    # drop complete duplicates

    housing_data_prep <- housing_data |>
        dplyr::distinct(.keep_all = TRUE)
    
    #--------------------------------------------------
    # generate spell across all deliveries
    
    housing_data_prep <- housing_data_prep |>
        # sort by obid and timing
        dplyr::arrange(obid, ajahr, amonat, ejahr, emonat) |>
        # after sorting stay within the obid (i.e. grouped)
        dplyr::group_by(obid)  |>
        # generate counting variable for group
        dplyr::mutate(
            spell = seq(1, dplyr::n())
        ) |>
        # ungroup to prevent potential issues in further steps
        dplyr::ungroup()

    #--------------------------------------------------
    # generate unique ID (counting ID)

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate(
            uniqueID_gen = seq(1, dplyr::n())
        )

    #--------------------------------------------------
    # generate dupID (duplicated ID)
    # identifying (potentially) duplicated listings

    housing_data_prep <- housing_data_prep |>
        dplyr::group_by(obid) |>
        dplyr::arrange(spell) |>
        dplyr::mutate(
            start_month = zoo::as.yearmon(paste0(ajahr, "-", amonat)),
            end_month = zoo::as.yearmon(paste0(ejahr, "-", emonat)),
            end_month_prev_spell = zoo::as.yearmon(dplyr::lag(end_month)),
            time_diff_prev_spell = (start_month - end_month_prev_spell) * 12,
            dplyr::across(
                .cols = c(
                    helpers_dupID_variables()[["continuous_set_1"]],
                    helpers_dupID_variables()[["continuous_set_2"]]
                ),
                ~ (abs(.x - dplyr::lag(.x)) / ((.x + dplyr::lag(.x)) / 2)) * 100,
                .names = "{.col}_diff"
            ),
            dplyr::across(
                .cols = c(
                    helpers_dupID_variables()[["categorical_set_1"]],
                    helpers_dupID_variables()[["categorical_set_2"]]
                ),
                ~ .x != dplyr::lag(.x),
                .names = "{.col}_diff"
            ),
            #--------------------------------------------------
            # defining identical observations
            identical_set_1 = dplyr::case_when(
                (
                    rowSums(
                        dplyr::across(
                            .cols = paste0(
                                helpers_dupID_variables()[["continuous_set_1"]],
                                "_diff"
                            ),
                            ~ .x == 0
                        )
                    ) == length(helpers_dupID_variables()[["continuous_set_1"]]) &
                    rowSums(
                        dplyr::across(
                            .cols = paste0(
                                helpers_dupID_variables()[["categorical_set_1"]],
                                "_diff"
                            ),
                            ~ .x == FALSE
                        )
                    ) == length(helpers_dupID_variables()[["categorical_set_1"]])
                ) ~ 1,
                TRUE ~ 0
            ),
            identical_set_2 = dplyr::case_when(
                (
                    rowSums(
                        dplyr::across(
                            .cols = paste0(
                                helpers_dupID_variables()[["categorical_set_2"]],
                                "_diff"
                            ),
                            ~ .x == 0
                        )
                    ) == length(helpers_dupID_variables()[["categorical_set_2"]]) &
                    rowSums(
                        dplyr::across(
                            .cols = paste0(
                                helpers_dupID_variables()[["categorical_set_2"]],
                                "_diff"
                            ),
                            ~ .x == FALSE
                        )
                    ) == length(helpers_dupID_variables()[["categorical_set_2"]])
                ) ~ 1,
                TRUE ~ 0
            ),
            #--------------------------------------------------
            # defining highly different observations
            exceeding_20_perc_diff = dplyr::case_when(
                rowSums( # rowSums computes the sum of TRUE values
                    dplyr::across(
                        .cols = paste0(
                            helpers_dupID_variables()[["critical_variables_20_perc"]],
                            "_diff"
                        ),
                        ~ .x > 20
                    )
                ) > 0 ~ 1,
                TRUE ~ 0
            ),
            exceeding_10_perc_diff = dplyr::case_when(
                rowSums(
                    dplyr::across(
                        .cols = paste0(
                            helpers_dupID_variables()[["critical_variables_10_perc"]],
                            "_diff"
                        ),
                        ~ .x > 10
                    )
                ) > 0 ~ 1,
                TRUE ~ 0
            ),
            highly_different = dplyr::case_when(
                exceeding_20_perc_diff == 1 | exceeding_10_perc_diff == 1 ~ 1,
                TRUE ~ 0
            ),
            #--------------------------------------------------
            # define dupID
            dupID_gen = NA,
            dupID_gen = dplyr::case_when(
                spell == 1 ~ 0,
                highly_different == 1 ~ 9,
                (
                    spell != 1 &
                    identical_set_1 == 1 &
                    identical_set_2 == 1 &
                    highly_different == 0 &
                    time_diff_prev_spell <= 6
                ) ~ 1,
                (
                    spell != 1 &
                    identical_set_1 == 1 &
                    identical_set_2 == 1 &
                    highly_different == 0 &
                    time_diff_prev_spell > 6
                ) ~ 2,
                (
                    spell != 1 &
                    identical_set_1 == 1 &
                    identical_set_2 == 0 &
                    highly_different == 0 &
                    time_diff_prev_spell <= 6
                ) ~ 3,
                (
                    spell != 1 &
                    identical_set_1 == 1 &
                    identical_set_2 == 0 &
                    highly_different == 0 &
                    time_diff_prev_spell > 6
                ) ~ 4,
                (
                    spell != 1 &
                    identical_set_1 == 0 &
                    identical_set_2 == 1 &
                    highly_different == 0 &
                    time_diff_prev_spell <= 6
                ) ~ 5,
                (
                    spell != 1 &
                    identical_set_1 == 0 &
                    identical_set_2 == 1 &
                    highly_different == 0 &
                    time_diff_prev_spell > 6
                ) ~ 6,
                (
                    spell != 1 &
                    identical_set_1 == 0 &
                    identical_set_2 == 0 &
                    highly_different == 0 &
                    time_diff_prev_spell <= 6
                ) ~ 7,
                (
                    spell != 1 &
                    identical_set_1 == 0 &
                    identical_set_2 == 0 &
                    highly_different == 0 &
                    time_diff_prev_spell > 6
                ) ~ 8
            )
        ) |>
        dplyr::select(-c(
            "start_month",
            "end_month",
            "end_month_prev_spell",
            dplyr::contains("exceeding"),
            dplyr::contains("diff"),
            dplyr::contains("identical")
        )) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # test for dupID that checks if there are missings in dupID

    targets::tar_assert_true(
        length(which(is.na(housing_data_prep$dupID_gen))) == 0,
        msg = glue::glue(
            "!!! WARNING: ",
            "DupID contains missings.",
            " (Error code: cad#1)"
        )
    )
    
    #--------------------------------------------------
    # return

    return(housing_data_prep)
}