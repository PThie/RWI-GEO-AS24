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
        "not_specified" = -7,
        "other" = -9,
        "all_missings" = c(-5, -7, -9)
    )

    #--------------------------------------------------
    # return

    return(missings)
}