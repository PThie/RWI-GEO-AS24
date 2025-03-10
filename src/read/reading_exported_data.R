reading_exported_data <- function(
    data_path = NA,
    file_format = NA,
    dependency = NA
) {
    #' @title Read exported data
    #' 
    #' @description This function reads the exported data from the data_path.
    #' 
    #' @param data_path Path to the exported data
    #' @param file_format File format of the exported data (character)
    #' @param dependency Object from previous step to indicate dependency between
    #' functions/ steps
    #' 
    #' @return Dataframe with the exported data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # actively call dependency to make sure that this function is executed if
    # something changes upstream

    targets::tar_assert_nonempty(
        dependency,
        msg = glue::glue(
            "The dependency object is empty.",
            " (Error code: red#1)"
        )
    )

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