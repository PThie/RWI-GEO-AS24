helpers_newer_variables <- function() {
    #' @title Defining newer variables
    #' 
    #' @description This function describes variables that only exist in later
    #' years (after 2022).
    #' 
    #' @return Character vector, variables that only exist in later years.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # newer variables

    vars <- c(
        "power",                    
        "transmissionid",
        "city",                 
        "country_zip_code",                   
        "askingprice",
        "bodycolorid",              
        "co2emissions",
        "cylinders",                
        "displacement",
        "efficiencyclassid",       
        "emissionpollutionclassid",
        "emissionstickerid",        
        "equipmentidfirst",
        "fuelconsumptioncity",
        "fuelconsumptioncountry",   
        "fuelconsumptionmixed",    
        "gears",                    
        "interiorcolorid",
        "numberofdoors",            
        "previousowners",           
        "seats",
        "vatdeductible",
        "weight"
    )

    # check that variables are not in deleted variables list
    # otherwise the pipeline breaks because you try to replace values for a 
    # variable that has been removed in a previous step
    for (var in vars) {
        if (var %in% helpers_deleted_variables()) {
            targets::tar_error(
                message = glue::glue(
                    "Variable {var} is in the deleted variables list. ",
                    "Please check if this is correct. (Error code: hnv#1)"
                ),
                class = "CustomError"
            )
        }
    }

    #--------------------------------------------------
    # return

    return(vars)
}
