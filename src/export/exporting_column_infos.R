exporting_column_infos <- function(
    auto_data = NA
) {
    #' @title Exporting column infos of first delivery
    #' 
    #' @description This function exports the column names and types of the
    #' first delivery as benchmark to check for consistency in the following
    #' deliveries.
    #' 
    #' @param auto_data Dataframe with original AS data
    #' 
    #' @return Dataframe with column names and types
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # export column names and types only for the very first delivery
    # (= benchmark)

    if (config_globals()[["current_delivery"]] == "Nov_2024") {
        #--------------------------------------------------
        # extract column types and names

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
                    " (Error code: cvn#1)"
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

        # export
        openxlsx::write.xlsx(
            coltypes,
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["current_version"]],
                "info",
                "column_types.xlsx"
            )
        )

        # return
        return(coltypes)
    }
}