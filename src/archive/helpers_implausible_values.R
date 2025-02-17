helpers_implausible_values <- function() {

    #--------------------------------------------------
    # implausible values

    ##### listing price
    implausible_price <- c(
        1231231, 1234123, 4444444, 5555555,
        6666666, 7777777, 8888888, 9191919,
        9999999
    )

    # maximum value refers to the most expensive car
    # most expensive car ca. 30 million (Rolls-Royce La Rose Noire Droptail)
    # https://www.wiwo.de/erfolg/trends/ranking-2024-das-sind-die-teuersten-autos-der-welt/27123208.html
    price_max_value <- 30000000

    ##### power
    # maximum value refers to strongest car
    # Devel Sixteen
    # https://www.carwow.co.uk/blog/highest-horsepower-cars#gref
    # https://domcar.com.cy/en/live/blog/top-10-most-powerful-cars-in-the-world/
    power_max_value <- 5000

    ##### city
    implausible_city <- c(
        ".",
        "....",
        "..........",
        "-"
    )

    ##### co2emissions
    implausible_co2emissions <- c(
        9999999,
        555555,
        999999,
        1111111,
        2222222,
        111111
    )

    ##### cylinders
    # https://www.auto-motor-und-sport.de/suv/cadillac-escalade-16-zylinder-motor/
    cylinders_max_value <- 16

    #--------------------------------------------------
    # combine all

    implausible_values <- list(
        "implausible_price" = implausible_price,
        "price_max_value" = price_max_value,
        "power_max_value" = power_max_value,
        "implausible_city" = implausible_city,
        "implausible_co2emissions" = implausible_co2emissions,
        "cylinders_max_value" = cylinders_max_value
    )

    #--------------------------------------------------
    # return

    return(implausible_values)
}