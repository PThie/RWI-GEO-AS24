reading_auto_data <- function(
    data_file_path = NA
) {
    #' @title Reading AutoScout24 data
    #' 
    #' @description This function reads all data from the delivery folder and
    #' combines it into one dataset.
    #' 
    #' @param data_file_path Character, path to the delivery folder.
    #' 
    #' @return Dataframe, combined data.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # list all files in delivery folder

    files <- list.files(
        data_file_path,
        pattern = "*.csv",
        full.names = TRUE
    )

    #--------------------------------------------------
    # read all data

    data_storage <- list()
    for (file in files) {
        dta <- data.table::fread(
            file,
            encoding = "UTF-8"
        )
        data_storage[[file]] <- dta
    }

    #--------------------------------------------------
    # check that all columns are in all partitions

    num_cols <- list()
    names_cols <- list()

    for (dta in names(data_storage)) {
        num_cols[[dta]] <- ncol(data_storage[[dta]])
        names_cols[[dta]] <- colnames(data_storage[[dta]])
    }

    targets::tar_assert_true(
        length(unique(num_cols)) == 1,
        msg = glue::glue(
            "!!! WARNING:
            Number of columns in all partitions is not the same: {unique(num_cols)}",
            " (Error code: rad#1)"
        )
    )

    targets::tar_assert_true(
        length(unique(names_cols)) == 1,
        msg = glue::glue(
            "!!! WARNING:
            Column names in all partitions are not the same: {unique(names_cols)}",
            " (Error code: rad#2)"
        )
    )
    
    #--------------------------------------------------
    # check variable types
    
    col_types_list <- list()
    for (dta in names(data_storage)) {
        col_types <- sapply(data_storage[[dta]], class) |>
            unlist() |>
            as.data.frame() |>
            dplyr::rename("col_type" = 1)
            
        # get column names
        col_types$col_name <- rownames(col_types)
        rownames(col_types) <- NULL

        # move column names to the front
        col_types <- col_types |>
            dplyr::relocate(col_name)

        # store
        col_types_list[[dta]] <- col_types
    }

    # combine all column types dataframes
    col_types_all <- purrr::reduce(
        col_types_list,
        dplyr::full_join,
        by = "col_name"
    )

    # check that all types are identical across partitions
    check_col_types <- col_types_all |>
        dplyr::rowwise() |>
        dplyr::mutate(
            all_equal = all(
                dplyr::c_across(-col_name) == dplyr::first(dplyr::c_across(-col_name))
            )
        ) |>
        dplyr::pull(all_equal)

    targets::tar_assert_true(
        unique(check_col_types) == TRUE,
        msg = "!!! WARNING:
        Column types are not the same across all partitions",
        " (Error code: rad#3)"
    )

    #--------------------------------------------------
    # combine all

    dta_prep <- data.table::rbindlist(
        data_storage,
        fill = TRUE
    )

    #--------------------------------------------------
    # return

    return(dta_prep)
}