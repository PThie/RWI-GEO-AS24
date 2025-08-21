helpers_missing_values <- function() {
    #' @title Define missing values
    #' 
    #' @description This function defines the missing values for the data set.
    #' 
    #' @return List of missing values
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define missing values

    missings <- list(
        "implausible" = -5,
        "not_available" = -6,
        "not_specified" = -7,
        "other" = -9,
        "all_missings" = c(-5, -6, -7, -9)
    )

    # check that you did not forget to add new missing values to all_missings
    targets::tar_assert_true(
        length(missings[["all_missings"]]) == length(missings) - 1,
        msg = glue::glue(
            "You have added a new missing value to the list. ",
            "Please add it to the all_missings list as well.",
            " (Error code: hmv#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(missings)
}