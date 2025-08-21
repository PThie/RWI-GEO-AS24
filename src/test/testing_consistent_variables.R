testing_consistent_variables <- function(
    auto_data = NA,
    column_types_benchmark = NA
) {
    #' @title Testing consistent variables
    #' 
    #' @description This function compares the variable names and types of the
    #' current version with that of the very first delivery. If there are
    #' differences, the function will return a list with the variables that need
    #' to be fixed.
    #' 
    #' @param auto_data Dataframe with original raw data.
    #' @param column_infos_benchmark Dataframe with column names and types of
    #' the very first delivery.
    #' 
    #' @note If variables have been fixed, add them to the list in config.
    #' 
    #' @return List with variables that need to be fixed.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # get column names and types of current delivery

    coltypes <- helpers_extracting_column_info(auto_data)

    #--------------------------------------------------
    # test for differences in names

    fix_columns <- c()

    for (col in coltypes$columns) {
        # check if column in in original set of variables (first delivery)
        if (!(col %in% column_types_benchmark$columns)) {
            if (!col %in% config_fixed_variables_consistency()[["fixed_names_vars"]]) {
                cli::cli_alert_danger(
                    cli::col_red(
                        "The following variable is not consistent with original data: ",
                        col
                    )
                )
                # store the variable that needs to be fixed
                fix_columns <- c(fix_columns, col)
            }
        }
    }

    #--------------------------------------------------
    # test for differences in types

    fix_types <- c()

    for (col in coltypes$columns) {
        # ignore country ID because this is equal to countrycode on later waves
        # (country ID will not be found in comparison with first delivery)
        # NOTE: difference in naming is already checked above and will be fixed
        # in cleaning_auto_data.R
        if (!col %in% c("country_id", "origin")) {
            # check if column type matches
            coltype_current <- coltypes |>
                dplyr::filter(columns == col) |>
                dplyr::select(columns_types) |>
                dplyr::pull()

            coltype_benchmark <- column_types_benchmark |>
                dplyr::filter(columns == col) |>
                dplyr::select(columns_types) |>
                dplyr::pull()

            if (coltype_current != coltype_benchmark) {
                if (!col %in% config_fixed_variables_consistency()[["fixed_types_vars"]]) {
                    cli::cli_alert_danger(
                        cli::col_red(
                            "The following variable has a different type than in the original data: ",
                            col
                        )
                    )
                    # store the variable that needs to be fixed
                    fix_types <- c(fix_types, col)
                }
            }
        }
    }

    #--------------------------------------------------
    # checks that stops the pipeline

    # test for consistent variables names
    targets::tar_assert_true(
        all(
            fix_columns %in% config_fixed_variables_consistency()[["fixed_names_vars"]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all variable names match the original data! ",
            "(Error code: tcv#1)"
        )
    )

    # test for consistent variable types
    targets::tar_assert_true(
        all(
            fix_types %in% config_fixed_variables_consistency()[["fixed_types_vars"]]
        ),
        msg = glue::glue(
            "!!! WARNING: ",
            "Not all variable types match the original data! ",
            "(Error code: tcv#2)"
        )
    )

    #--------------------------------------------------
    # combine both vectors for export

    fix <- list(
        "columns" = fix_columns,
        "types" = fix_types
    )

    #--------------------------------------------------
    # return

    return(fix)
}