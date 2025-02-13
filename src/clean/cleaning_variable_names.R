cleaning_variable_names <- function(
    auto_data = NA
) {
    #' @title Cleaning variable names
    #' 
    #' @description This function cleans the variable names of the AutoScout24
    #' data set.
    #' 
    #' @param auto_data Dataframe with AutoScout24 data
    #' 
    #' @return Dataframe with cleaned variable names
    #' @author Patrick Thiel

    #--------------------------------------------------

    auto_data <- auto_data |>
        dplyr::rename(
            ofid = classifiedguid,
            brand = makeid,
            model = modelid,
            first_registration = firstregistration,
            transmission = transmissionid,
            provider_type = customertypeid,
            asking_price = askingprice,
            created_date = createddate,
            body_color = bodycolorid,
            vehicle_type = bodytypeid,
            co2_emissions = co2emissions,
            country_code = countrycode,
            efficiency_class = efficiencyclassid,
            emission_class = emissionpollutionclassid,
            emission_sticker = emissionstickerid,
            equipment_first = equipmentfirst,
            fuel_consumption_city = fuelconsumptioncity,
            fuel_consumption_rural = fuelconsumptioncountry,
            fuel_consumption_mixed = fuelconsumptionmixed,
            fuel_type = fuelid,
            interior_color = interiorcolorid,
            num_doors = numberofdoors
            num_previous_owners = previousowners,
            state_id = stateid,
            vat_deductible = vatdeductible
        )

    #--------------------------------------------------
    # return

    return(auto_data)
}