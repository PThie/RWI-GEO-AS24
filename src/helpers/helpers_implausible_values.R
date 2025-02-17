helpers_implausible_values <- function() {
    #' @title Define implausible values
    #' 
    #' @description This function defines the implausible values for the dataset.
    #' 
    #' @return List with implausible values
    #' @author Patrick Thiel

    #--------------------------------------------------
    # implausible values

    ##### city
    implausible_city <- c(
        ".",
        "....",
        "..........",
        "-"
    )

    #--------------------------------------------------
    # combine all

    implausible_values <- list(
        "implausible_price" = implausible_price
    )

    #--------------------------------------------------
    # return

    return(implausible_values)
}