creating_folder_structure <- function() {
    #' @title Create folder structure
    #' 
    #' @description This function creates the folder structure for the output
    #' of a new wave.
    #' 
    #' @return NULL
    #' @author Patrick Thiel

    #--------------------------------------------------
    # create main folder

    if (!dir.exists(config_paths()[["output_path"]])) {
        dir.create(
            config_paths()[["output_path"]],
            recursive = TRUE
        )
    }
    #--------------------------------------------------
    # create type folders with subdirectories

    for (folder in config_globals()[["folders"]]) {
        directory <- file.path(
            config_paths()[["output_path"]],
            folder
        )

        if (!dir.exists(directory)) {
            dir.create(directory, recursive = TRUE)
        }
    }

    #--------------------------------------------------
    # return

    return(NULL)
}