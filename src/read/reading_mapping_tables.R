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

    if (config_globals()[["current_delivery"]] == "Nov_2024") {
        mapping_tables_list <- list()

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

            # replace special characters
            dta <- dta |>
                dplyr::mutate(
                    mapping_value = stringi::stri_trans_general(
                        mapping_value,
                        "de-ASCII; Latin-ASCII"
                    )
                )

            # fix that "private" is labeled "privat" in customertypeid
            if (name == "customertype") {
                dta <- dta |>
                    dplyr::mutate(
                        mapping_value = dplyr::case_when(
                            mapping_value == "Privat" ~ "Private",
                            TRUE ~ mapping_value
                        )
                    )
            }

            # NOTE: some of the variables have different names in the dataset
            if (name == "customertype") {
                name <- "customertypeid"
            } else if (name == "efficiencyclass") {
                name <- "efficiencyclassid"
            } else if (name == "emissionclassid") {
                name <- "emissionpollutionclassid"
            } else if (name == "equipmentid") {
                name <- "equipmentidfirst"
            }
            
            # store
            mapping_tables_list[[name]] <- dta
        }

        # export list for further use
        # NOTE: version is hard coded because the version in globals changes
        saveRDS(
            mapping_tables_list,
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "mapping_tables_list.rds"
            )
        )
    } else {
        #--------------------------------------------------
        # read mapping tables from benchmark

        mapping_tables_list <- readRDS(
            file.path(
                config_paths()[["output_path"]],
                "v1",
                "info",
                "mapping_tables_list.rds"
            )
        )
    }

    #--------------------------------------------------
    # test that output list is not empty

    targets::tar_assert_nonempty(
        mapping_tables_list,
        msg = glue::glue(
            "!!! WARNING:",
            " Mapping tables list is empty.",
            " (Error code: rmt#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(mapping_tables_list)
}