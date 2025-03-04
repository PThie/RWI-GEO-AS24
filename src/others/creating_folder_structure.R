creating_folder_structure <- function() {
    #' @title Create folder structure
    #' 
    #' @description This function creates the folder structure for the output
    #' of a new wave.
    #' 
    #' @return NULL
    #' @author Patrick Thiel

    #--------------------------------------------------
    # create output folder

    if (!dir.exists(config_paths()[["output_path"]])) {
        dir.create(
            config_paths()[["output_path"]],
            config_globals()[["next_version"]],
            recursive = TRUE
        )
    }

    #--------------------------------------------------
    # create subdirectories within output folder

    for (folder in config_globals()[["output_folders"]]) {
        directory <- file.path(
            config_paths()[["output_path"]],
            config_globals()[["next_version"]],
            folder
        )

        if (!dir.exists(directory)) {
            dir.create(directory, recursive = TRUE)
        }
    }

    #--------------------------------------------------
    # create data folders

    for (folder in config_globals()[["data_folders"]]) {
        directory <- file.path(
            config_paths()[["data_path"]],
            folder
        )

        if (!dir.exists(directory)) {
            dir.create(directory, recursive = TRUE)
        }
    }

    #--------------------------------------------------
    # create subdirectories within data folders

    directory_suf <- file.path(
        config_paths()[["data_path"]],
        "SUF",
        config_globals()[["next_version"]]
    )

    if (!dir.exists(directory_suf)) {
        dir.create(directory_suf, recursive = TRUE)
    }

    directory_processed <- file.path(
        config_paths()[["data_path"]],
        "processed",
        paste0(
            "Lieferung_",
            config_globals()[["current_delivery"]]
        )
    )

    if (!dir.exists(directory_processed)) {
        dir.create(directory_processed, recursive = TRUE)
    }

    #--------------------------------------------------
    # create folders for exported data

    for (file_format in config_globals()[["exported_file_formats"]]) {
        directory <- file.path(
            config_paths()[["data_path"]],
            "SUF",
            config_globals()[["next_version"]],
            file_format
        )

        if (!dir.exists(directory)) {
            dir.create(directory, recursive = TRUE)
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}