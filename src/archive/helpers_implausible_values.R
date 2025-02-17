helpers_implausible_values <- function() {

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