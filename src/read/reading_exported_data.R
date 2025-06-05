reading_exported_data <- function(
    data_path = NA,
    file_format = NA,
    dependencies = NA
) {
    #' @title Read exported data
    #' 
    #' @description This function reads the exported data from the data_path.
    #' 
    #' @param data_path Path to the exported data
    #' @param file_format File format of the exported data (character)
    #' @param dependencies List of objects from previous step to indicate
    #' dependency between functions/ steps
    #' 
    #' @return Dataframe with the exported data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # check dependency
    # NOTE: do something with dependency, so it actually matters that you have
    # passed it

    for (obj in dependencies) {
        targets::tar_assert_nonempty(
            obj,
            msg = glue::glue(
                "!!! ERROR: ",
                "The dependency is empty.",
                " (Error code: red#1)"
            )
        )
    }

    #--------------------------------------------------
    # test that file formats did not change since the last time
    # otherwise, more reading blocks have to be added

    targets::tar_assert_true(
        all(c("parquet", "csv") %in% helpers_target_names()[["exported_file_formats"]]),
        msg = glue::glue(
            "!!! WARNING: ",
            "The file formats for the exported data have changed.",
            " (Error code: red#2)"
        )
    )
    
    #--------------------------------------------------
    # read data according to file format

    if (file_format == "csv") {
        dta <- data.table::fread(
            data_path,
            sep = ";"
        )
    } else {
        dta <- arrow::read_parquet(data_path)
    }

    #--------------------------------------------------
    # return

    return(dta)
    
}