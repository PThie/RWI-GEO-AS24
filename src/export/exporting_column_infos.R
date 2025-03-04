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

        openxlsx::write.xlsx(
            coltypes,
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["next_version"]],
                "info",
                "column_types.xlsx"
            )
        )

        #--------------------------------------------------
        # return

        return(coltypes)
    }
}