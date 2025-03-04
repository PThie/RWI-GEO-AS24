exporting_value_labels <- function(
    auto_data = NA
) {
    #' @title Exporting value labels
    #' 
    #' @description This function exports the value labels of the mapped variables.
    #' 
    #' @param auto_data Dataframe with cleaned and mapped auto data
    #' 
    #' @return List with unique values for each mapping variable
    #' @author Patrick Thiel

    #--------------------------------------------------
    # extract unique values for each mapping variable

    unique_values_list <- list()
    for (var in config_globals()[["needs_mapping_vars"]]) {
        if (var %in% names(auto_data)) {
            # extract all unique values for variable of interest
            unique_values <- auto_data |>
                dplyr::distinct(.data[[var]]) |>
                dplyr::pull()
            
            unique_values <- unique_values[
                !unique_values %in% as.character(
                    helpers_missing_values()[["all_missings"]]
                )
            ]

            # define export path
            directory <- file.path(
                config_paths()[["output_path"]],
                config_globals()[["next_version"]],
                "info",
                "value_labels.txt"
            )

            # define header for export
            header <- glue::glue(
                "
                #--------------------------------------------------
                Values for {var}
                #--------------------------------------------------
                "
            )

            # export header
            write(
                header,
                directory,
                append = TRUE
            )

            # export value
            gdata::write.fwf(
                unique_values |> as.data.frame(),
                directory,
                append = TRUE,
                rownames = FALSE,
                colnames = FALSE
            )

            # store
            unique_values_list[[var]] <- unique_values
        }
    }

    #--------------------------------------------------
    # return

    return(unique_values_list)
}