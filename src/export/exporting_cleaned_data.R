exporting_cleaned_data <- function(
    auto_data = NA
) {
    #' @title Exporting cleaned data
    #' 
    #' @description This function exports the cleaned data to a parquet file.
    #' The export is delivery specific, i.e. each delivery has its own folder.
    #' Needed to append all data deliveries into one dataset.
    #' 
    #' @param auto_data Dataframe with cleaned auto data
    #' 
    #' @return Dataframe with exported data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # export data

    arrow::write_parquet(
        auto_data,
        file.path(
            config_paths()[["data_path"]],
            "processed",
            paste0("Lieferung_", config_globals()[["current_delivery"]]),
            "clean_data.parquet"
        )
    )

    #--------------------------------------------------
    # return

    return(auto_data)
}