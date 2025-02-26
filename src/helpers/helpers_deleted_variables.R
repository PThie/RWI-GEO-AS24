helpers_deleted_variables <- function() {
    #' @title Define variables to be removed
    #' 
    #' @description This function defines the variables to be removed from the
    #' data set that are potentially in the data set because of internal process
    #' at AutoScout24.
    #' 
    #' @return Vector with variables to be removed
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # define variables to be removed

    cols <- c(
        "rn", # Internally used to select most recent version of listing. no external value
        "year", # because part of partition_date
        "month", # because part of partition_date
        "day", # because part of partition_date
        "fornewmarket", # because should be always 0 since our data is only used cars
        "currencyid", # because all prices are in Euros
        "visibilitytypeid", # internal variable for AutoScout
        "offertypeid", # because only contains U = used as we only get used cars from AS24
        "partition_date", # because only needed for AS24 to see when the data was selected for delivery
        "stateid" # because only contains 'A'
    )

    #--------------------------------------------------
    # return

    return(cols)

}