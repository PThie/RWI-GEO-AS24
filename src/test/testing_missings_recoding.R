testing_missings_recoding <- function(
    auto_data = NA,
    file_format = NA
) {
    #' @title Test missings recoding
    #' 
    #' @description This function tests if the missings in the exported data
    #' have been recoded properly.
    #' 
    #' @param auto_data Dataframe with the exported data
    #' @param file_format File format of the data (character)
    #' 
    #' @return Dataframe with the number of missing values for each variable
    #' @author Patrick Thiel

    #--------------------------------------------------
    # calculate the number of missing values for each variable

    missings <- sapply(auto_data, function(x) sum(is.na(x))) |>
        as.data.frame() |>
        dplyr::rename(n_missing = 1)

    missings$variables <- rownames(missings)
    rownames(missings) <- NULL

    #--------------------------------------------------
    # test if number of missings is zero

    for (var in missings$variables) {
        targets::tar_assert_true(
            missings$n_missing[missings$variables == var] == 0,
            msg = glue::glue(
                "There are missings that are not recoded in the variable {var}.",
                " (Error code: tmr#1)"
            )
        )
    }

    #--------------------------------------------------
    # return

    return(missings)
}