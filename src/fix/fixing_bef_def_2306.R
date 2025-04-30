fixing_bef_del_2306 <- function() {
    #' @title Fixing befeuerungsarten for delivery 2306
    #' 
    #' @description This function fixes the befeuerungsarten variable for the
    #' delivery 2306. The issue was the bef variables were wrong in this delivery
    #' and had been fixed during the cleaning of delivery 2312.
    #' 
    #' @note See also issue #32 on GitHub for more details.
    #' 
    #' @return Dataframe with the fixed befeuerungsarten variable.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read raw data for delivery 2023

    raw_data <- data.table::fread(
        file.path(
            config_paths()[["data_path"]],
            "original",
            "Lieferung_2306",
            "commercial_data_all.csv"
        )
    )

    #--------------------------------------------------
    # read processed data (as published as V1)

    processed_data <- arrow::read_parquet(
        file.path(
            config_paths()[["data_path"]],
            "processed",
            "Lieferung_2306",
            "clean_data.parquet"
        )
    )

    #--------------------------------------------------
    # prepare data
    # cleaning steps are taken from cleaning_org_data.R

    # make all names lowercase
    names(raw_data) <- tolower(names(raw_data))

    # apply cleaning steps
    housing_data_prep <- raw_data |>
        dplyr::select(obid, zeitraum, befeuerungsarten) |>
        dplyr::mutate(
            # NOTE: data variable needed for merging
            #--------------------------------------------------
            # split date variable
            # starting year and month
            ajahr = as.numeric(substring(zeitraum, first = 1, last = 4)),
            amonat = as.integer(substring(zeitraum, first = 5, last = 7)),
            # end year and month
            ejahr = as.numeric(substring(zeitraum, first = 12, last = 15)),
            emonat = as.integer(substring(zeitraum, first = 16, last = 17)),
            #--------------------------------------------------
            # replace NAs in befeuerungsarten
            befeuerungsarten = dplyr::case_when(
                befeuerungsarten == "" ~ NA_character_,
                TRUE ~ befeuerungsarten
            ),
            # generate bef1 which contains the firing type if there is only one
            # given
            # NOTE: cannot be done in one step because of the warning issue
            # "insert NAs because of coercion to numeric" which crashes the
            # pipeline
            bef1 = dplyr::case_when(
                stringr::str_detect(befeuerungsarten, "\\|") ~ NA_character_,
                TRUE ~ befeuerungsarten
            ),
            bef1 = as.numeric(bef1),
            # generate help variable which is missing when bef1 has a value
            bef_help = dplyr::case_when(
                !is.na(bef1) ~ NA,
                TRUE ~ befeuerungsarten
            ),
            # replace help variable with splitted values (by "|")
            # output column will be of type list
            bef_help = stringr::str_split(bef_help, "\\|"),
            # get the number of times "|" occured
            bef_count = stringr::str_count(befeuerungsarten, "\\|")
        ) |>
        # split bef_help column into separate columns and generate new names
        tidyr::unnest_wider(bef_help, names_sep = "_") |>
        # set as dataframe
        as.data.frame()

    #--------------------------------------------------
    # rename columns "bef" (befeuerungsarten)

    # get the max amount of potential splits based on delimiter "|"
    max_split <- max(housing_data_prep$bef_count, na.rm = TRUE)

    # extract original names (names to leave unchanged)
    # exception of bef1
    org_names <- housing_data_prep |>
        dplyr::select(!dplyr::contains("bef_help") & !dplyr::contains("bef_count")) |>
        names()

    # generate new names
    # why plus 2: because we start at 2 (bef1 already exists) and the first
    # split breaks into two pieces
    new_names <- c()
    for (i in seq(2, max_split + 2)) {
        nam <- paste0("bef", i)
        # update empty list of names
        new_names <- c(new_names, nam)
    }

    # assign new names
    names(housing_data_prep) <- c(org_names, new_names, "bef_count")

    #--------------------------------------------------
    # drop unnecessary columns

    housing_data_prep <- housing_data_prep |>
        dplyr::select(-c(
            "bef_count", # auxiliary variable from previous step
            "befeuerungsarten", # transformed to bef1, bef2, ...
            "zeitraum" # transformed to ajahr, amonat, ejahr, emonat
        ))

    #--------------------------------------------------
    # setting correct types
    # sometimes it is already correct by forcing it here again

    # integer columns
    bef_cols <- housing_data_prep |>
        dplyr::select(dplyr::starts_with("bef")) |>
        names()
    
    int_cols <- c(
        bef_cols,
        "ejahr", "emonat", "ajahr", "amonat"
    )

    # character columns
    char_cols <- c(
        "obid"
    )

    # Test that all columns are covered
    all_variables <- c(
        int_cols,
        char_cols
    )

    for (var in names(housing_data_prep)) {
        targets::tar_assert_true(
            var %in% all_variables,
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable {var} not covered in the type setting. ",
                "(Error code: fbd#1)"
            )
        )
    }

    # set types
    for (col in int_cols) {
        # NOTE: ignore columns that have been deleted. Otherwise, they would be
        # added here again
        if (!col %in% helpers_deleted_variables()) {
            if (col %in% names(housing_data_prep)) {
                housing_data_prep[[col]] <- as.integer(housing_data_prep[[col]])
            } else {
                housing_data_prep[[col]] <- helpers_missing_values()[["other"]]
            }
        }
    }

    for (col in char_cols) {
        if (!col %in% helpers_deleted_variables()) {
            if (col %in% names(housing_data_prep)) {
                housing_data_prep[[col]] <- as.character(housing_data_prep[[col]])
            } else {
                housing_data_prep[[col]] <- as.character(helpers_missing_values()[["other"]])
            }
        }
    }

    #--------------------------------------------------
    # replace missings according to type

    housing_data_prep <- housing_data_prep |>
        dplyr::mutate_if(
            is.integer,
            replace_na,
            replace = helpers_missing_values()[["other"]]
        ) |>
        dplyr::mutate_if(
            is.numeric,
            replace_na,
            replace = helpers_missing_values()[["other"]]
        )  |>
        dplyr::mutate_if(
            is.character,
            replace_na,
            replace = as.character(
                helpers_missing_values()[["other"]]
            )
        )

    #--------------------------------------------------
    # merge processed data with newly cleaned raw data

    # drop "wrong" bef columns from processed data
    old_bef_cols <- processed_data |>
        dplyr::select(dplyr::contains("bef")) |>
        names()

    targets::tar_assert_true(
        all(old_bef_cols %in% c("bef1", "bef2")),
        msg = glue::glue(
            "!!! WARNING: ",
            "Old data contains more than two bef columns. ",
            "(Error code: fbd#2)"
        )
    )

    processed_data_prep <- processed_data |>
        dplyr::select(-dplyr::all_of(old_bef_cols))

    # extract number of rows before merge
    n_rows_before <- nrow(processed_data_prep)

    # merge both datasets
    processed_data_prep <- processed_data_prep |>
        merge(
            housing_data_prep,
            by = c("obid", "ajahr", "amonat", "emonat", "ejahr"),
            all.x = TRUE
        )

    # check if all rows are still there
    targets::tar_assert_true(
        nrow(processed_data_prep) == n_rows_before,
        msg = glue::glue(
            "!!! WARNING: ",
            "Number of rows in processed data is not the same as before. ",
            "(Error code: fbd#3)"
        )
    )

    #--------------------------------------------------
    # export

    arrow::write_parquet(
        processed_data_prep,
        file.path(
            config_paths()[["data_path"]],
            "processed",
            "Lieferung_2306",
            "fixed_bef",
            "clean_data_fixed_bef.parquet"
        )
    )

    #--------------------------------------------------
    # return

    return(processed_data_prep)
}