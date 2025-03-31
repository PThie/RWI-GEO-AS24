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
        "year",                     
        "month",
        "day",                      
        "askingprice",
        "bodycolorid",              
        "co2emissions",
        "countrycode",              
        "currencyid",
        "cylinders",                
        "displacement",
        "efficiencyclassid",       
        "emissionpollutionclassid",
        "emissionstickerid",        
        "equipmentidfirst",
        "fornewmarket",             
        "fuelconsumptioncity",
        "fuelconsumptioncountry",   
        "fuelconsumptionmixed",    
        "gears",                    
        "interiorcolorid",
        "numberofdoors",            
        "offertypeid",
        "previousowners",           
        "seats",
        "stateid",                  
        "vatdeductible",
        "visibilitytypeid",         
        "weight"
    )

    #--------------------------------------------------
    # return

    return(vars)
}
