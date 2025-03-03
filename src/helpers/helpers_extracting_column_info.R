helpers_extracting_column_info <- function(
    auto_data = NA
) {
    #' @title Extract column information from a dataset
    #' 
    #' @description This function extracts column information from a dataset.
    #' 
    #' @param auto_data Dataframe with AutoScout data.
    #' 
    #' @return Dataframe with column names and types.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # get column names and types

    coltypes <- sapply(auto_data, class) |>
        unlist() |>
        as.data.frame() |>
        dplyr::rename(columns_types = 1)

    coltypes$columns <- rownames(coltypes)
    rownames(coltypes) <- NULL

    #--------------------------------------------------
    # handle date columns

    date_columns <- c(
        "firstregistration",
        "createddate",
        "partition_date"
    )

    # test that columns are actually in the dataset
    for (col in date_columns) {
        targets::tar_assert_true(
            col %in% colnames(auto_data),
            msg = glue::glue(
                "{col} not in the dataset.",
                " (Error code: heci#1)"
            )
        )
    }

    coltypes <- coltypes |>
        dplyr::relocate(columns) |>
        # handle that dates have two types
        dplyr::filter(
            !columns %in% paste0(date_columns, "2")
        ) |>
        dplyr::mutate(
            columns = stringr::str_replace(columns, "[0-9]", "")
        )

    #--------------------------------------------------
    # return

    return(coltypes)
}