helpers_fixing_reading_types <- function(
    data_names = NA,
    data_label = NA,
    data_storage_list = NA,
    target_var = NA_character_,
    desired_type = NA_character_
) {
    #' @title Fixing reading types
    #' 
    #' @description This function fixes differences in column types across data
    #' partitions. This prevents that types get mixed when the data is rowbind
    #' into one dataset.
    #' 
    #' @param data_names Character vector with data file names to consider.
    #' @param data_label Label for the data to be checked. Eases the identification
    #' if the test fails.
    #' @param data_storage_list List with data storage, i.e. read in raw data.
    #' @param target_var Character with variable name to be transformed.
    #' @param desired_type Character with the desired type of the target variable.
    #' 
    #' @return List with data storage where the target variable has the desired type.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # transform the type of the target variable in all data partitions
    
    all_types <- c()
    for (dta in data_names) {
        data_storage_list[[dta]][[target_var]] <- as(
            data_storage_list[[dta]][[target_var]],
            desired_type
        )

        # store the type of target variable
        all_types <- c(
            all_types,
            typeof(data_storage_list[[dta]][[target_var]])
        )
    }

    #--------------------------------------------------
    # test that the transformation has worked and only one type is present

    targets::tar_assert_true(
        length(unique(all_types)) == 1,
        msg = glue::glue(
            "!!! WARNING:
            Column types are not the same across all partitions for {target_var}.",
            " Check {data_label}.",
            " (Error code: hfrt#1)"
        )
    )

    #--------------------------------------------------
    # return
    
    return(data_storage_list)
}