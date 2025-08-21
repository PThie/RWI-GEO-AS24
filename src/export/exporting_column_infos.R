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

        coltypes <- helpers_extracting_column_info(auto_data)

        #--------------------------------------------------
        # export
        # NOTE: version is hard coded because the version in globals changes

        openxlsx::write.xlsx(
            coltypes,
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "column_types.xlsx"
            )
        )
    } else {
        #--------------------------------------------------
        # read column types from benchmark
        # NOTE: version is hard coded because the version in globals changes

        coltypes <- openxlsx::read.xlsx(
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "column_types.xlsx"
            )
        )
    }

    #--------------------------------------------------
    # test that dataset is not empty

    targets::tar_assert_nonempty(
        coltypes,
        msg = glue::glue(
            "!!! WARNING:",
            " Column types dataset is empty.",
            " (Error code: eci#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(coltypes)
}