reading_mapping_tables <- function() {
    #' @title Reading mapping tables
    #' 
    #' @description This function reads the mapping tables for the AutoScout24
    #' dataset.
    #' 
    #' @return List with mapping tables
    #' @author Patrick Thiel

    #--------------------------------------------------
    # NOTE: Only read for Nov 2024 delivery because the mapping tables stay
    # constant across all deliveries (and there are no new mapping tables for
    # other deliveries).

    mapping_tables_list <- list()
    if (config_globals()[["current_delivery"]] == "Nov_2024") {
        # list all mapping tables
        mapping_tables_files <- list.files(
            file.path(
                config_paths()[["data_path"]],
                "raw",
                config_globals()[["current_delivery"]],
                "mapping tables"
            ),
            pattern = "\\.csv$",
            full.names = TRUE,
            ignore.case = TRUE
        )

        # create file name
        file_names_aux <- stringr::str_remove_all(
            mapping_tables_files,
            pattern = file.path(
                    config_paths()[["data_path"]],
                    "raw",
                    config_globals()[["current_delivery"]],
                    "mapping tables",
                    "map_"
                )
        )

        file_names <- stringr::str_remove_all(
            file_names_aux,
            pattern = ".csv"
        )

        # read all mapping tables
        for (file in mapping_tables_files) {
            # extract name
            name <- file_names[
                stringr::str_detect(file, file_names)
            ]

            # read data
            dta <- data.table::fread(
                file,
                header = TRUE
            )

            # delete running ID if existent
            if ("V1" %in% names(dta)) {
                dta <- dta |>
                    dplyr::select(-V1)
            }

            # make column names uniform
            id_name <- names(dta)[stringr::str_detect(names(dta), "id")]
            non_id_name <- names(dta)[stringr::str_detect(names(dta), id_name) == FALSE]

            dta <- dta |>
                dplyr::rename(
                    mapping_id = id_name,
                    mapping_value = non_id_name
                )

            # store
            mapping_tables_list[[name]] <- dta
        }
    }

    #--------------------------------------------------
    # return

    return(mapping_tables_list)
}