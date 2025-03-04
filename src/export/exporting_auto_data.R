exporting_auto_data <- function(
    auto_data = NA
) {
    #' @title Exporting auto data
    #' 
    #' @description This function exports the cleaned and mapped auto data.
    #' 
    #' @param auto_data Dataframe with cleaned and mapped auto data
    #' 
    #' @return Dataframe with cleaned and mapped auto data
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define name of exported dataset

    suf_name <- paste0("CARMKT_", config_globals()[["next_version"]], "_SUF")

    #--------------------------------------------------
    # export data

    for (file_format in config_globals()[["exported_file_formats"]]) {
        if (file_format == "csv") {
            data.table::fwrite(
                auto_data,
                file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["next_version"]],
                    file_format,
                    paste0(suf_name, ".csv")
                ),
                row.names = FALSE,
                sep = ";"
            )
        } else {
            arrow::write_parquet(
                auto_data,
                file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["next_version"]],
                    file_format,
                    paste0(suf_name, ".parquet")
                )
            )
        }
    }

    #--------------------------------------------------
    # return

    return(auto_data)
}