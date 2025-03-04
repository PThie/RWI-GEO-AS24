testing_deleted_variables <- function(
    auto_data = NA,
    file_format = NA
) {
    #' @title Test variable deletion
    #' 
    #' @description This function tests if the exported data is compliant with
    #' the requirements, i.e. if all variables have been deleted that are 
    #' supposed to be deleted.
    #' 
    #' @param auto_data Dataframe with the exported data
    #' @param file_format File format of the data (character)
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # test if all variables have been deleted that are supposed to be deleted

    for (col in helpers_deleted_variables()) {
        targets::tar_assert_true(
            !(col %in% names(auto_data)),
            msg = glue::glue(
                "!!! WARNING: ",
                "Variable '{col}' is still present in the {file_format} data.",
                " (Error code: sct#1)"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(NULL)
}