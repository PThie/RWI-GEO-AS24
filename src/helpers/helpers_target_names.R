helpers_target_names <- function() {
    #' @title Create target names
    #' 
    #' @description This function creates a list of target names used in the
    #' pipeline when dynamic branching is used (i.e. when tar_eval is used).
    #'  
    #' @return List, target names
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # list of target names

    # NOTE: define outside list to be able to use in list
    exported_file_formats <- config_globals()[["exported_file_formats"]]

    target_names <- list(
        #--------------------------------------------------
        # names for testing
        "exported_file_formats" = exported_file_formats,
        "suf_exported_data" = glue::glue(
            "suf_exported_data_{exported_file_formats}"
        ),
        "deleted_variables_test" = glue::glue(
            "deleted_variables_test_{exported_file_formats}"
        )
    )

    #--------------------------------------------------
    # return

    return(target_names)
}