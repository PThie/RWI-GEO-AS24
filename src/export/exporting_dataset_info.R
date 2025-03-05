exporting_dataset_info <- function(
    auto_data = NA
) {
    #' @title Exporting dataset information
    #' 
    #' @description This function collects information about the dataset and
    #' exports it to a text file for reporting.
    #' 
    #' @param auto_data Dataframe with cleaned auto data.
    #' 
    #' @return Dataframe with information about the dataset.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # collect information about the dataset

    num_rows <- nrow(auto_data)
    num_cols <- ncol(auto_data)
    col_names <- colnames(auto_data)
    included_countries <- unique(auto_data$country_code)

    # combine all information
    infos <- as.data.frame(
        cbind(
            Variables = c(
                "Number of rows:",
                "Number of columns:",
                "Column names:",
                "Included countries:"
            ),
            Values = c(
                num_rows,
                num_cols,
                paste(col_names, collapse = ", "),
                paste(included_countries, collapse = ", ")
            )
        )
    )

    # define export path
    directory <- file.path(
        config_paths()[["output_path"]],
        config_globals()[["next_version"]],
        "info",
        "dataset_info.txt"
    )

    # export dates
    gdata::write.fwf(
        infos,
        directory,
        append = TRUE,
        rownames = FALSE,
        colnames = FALSE,
        quote = FALSE
    )

    #--------------------------------------------------
    # return

    return(infos)
}