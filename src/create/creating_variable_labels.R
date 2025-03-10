creating_variable_labels <- function(
    auto_data = NA
) {
    #' @title Generating variable labels
    #' 
    #' @description This function generates labels for variables for documentation.
    #' 
    #' @param auto_data Dataframe with cleaned auto data
    #' 
    #' @return Dataframe with variable labels
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # extract variable names

    var_names <- names(auto_data) |>
        sort()

    # write into dataframe
    label_data <- data.frame(
        "variable" = var_names
    )

    #--------------------------------------------------
    # create category

    label_data <- label_data |>
        dplyr::mutate(
            "category" = NA_character_,
            "category" = dplyr::case_when(
                variable %in% c(
                    "country_zip_code",
                    "country_code",
                    "city",
                    "zipcode"
                ) ~ "Regional information",
                variable %in% c(
                    "created_date",
                    "first_registration"
                ) ~ "Temporal information",
                variable %in% c(
                    "price",
                    "asking_price",
                    "vat_deductible"
                ) ~ "Features related to price",
                variable %in% c(
                    "carmkt_delivery",
                    "carmkt_version"
                ) ~ "Generated features",
                variable %in% c(
                    "ofid",
                    "uniqueID_gen"
                ) ~ "Identifiers",
                variable %in% c(
                    "co2_emissions",
                    "fuel_type",
                    "fuel_consumption_city",
                    "fuel_consumption_rural",
                    "fuel_consumption_mixed",
                    "efficiency_class",
                    "emission_class",
                    "emission_sticker"
                ) ~ "Features related to fuel and emissions",
                variable %in% c(
                    "power",
                    "cylinders",
                    "gears",
                    "displacement",
                    "transmission"
                ) ~ "Features related to performance",
                variable %in% c(
                    "brand",
                    "mileage",
                    "model",
                    "weight",
                    "vehicle_type"
                ) ~ "Vehicle characteristics",
                variable %in% c(
                    "body_color",
                    "num_doors"
                ) ~ "Features related to exterior",
                variable %in% c(
                    "equipment_first",
                    "interior_color",
                    "seats"
                ) ~ "Features related to interior",
                variable %in% c(
                    "provider_type",
                    "num_previous_owners"
                ) ~ "Other features"
            )
        )

        # Check that no variable was not classified
        targets::tar_assert_true(
            length(which(is.na(label_data$category))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not classified.",
                " (Error code: cvl#1)"
            )
        )

        #--------------------------------------------------
        # create labels

        label_data <- label_data |>
            dplyr::mutate(
                label = NA_character_,
                label = dplyr::case_when(
                    variable == "asking_price" ~ "Indicator for price being asking price",
                    variable == "body_color" ~ "Body color of the car",
                    variable == "brand" ~ "Brand of the car",
                    variable == "carmkt_delivery" ~ "Classification of data delivery period",
                    variable == "carmkt_version" ~ "Classification of data version",
                    variable == "city" ~ "City of the seller",
                    variable == "co2_emissions" ~ "CO2 emissions of the car (in g/km)",
                    variable == "country_code" ~ "Label for country of the seller",
                    variable == "country_zip_code" ~ "Country zip-code combination of the seller",
                    variable == "created_date" ~ "Creation date of the listing",
                    variable == "cylinders" ~ "Number of cylinders of the car",
                    variable == "displacement" ~ "Displacement of the car (in cc)",
                    variable == "efficiency_class" ~ "Efficiency class of the car",
                    variable == "emission_class" ~ "Emission class of the car",
                    variable == "emission_sticker" ~ "Emission sticker of the car",  
                    variable == "equipment_first" ~ "First listed equipment of the car",
                    variable == "first_registration" ~ "First registration date of the car",
                    variable == "fuel_consumption_city" ~ "Fuel consumption within city (in l/km)",
                    variable == "fuel_consumption_mixed" ~ "Mixed fuel consumption (in l/km)",
                    variable == "fuel_consumption_rural" ~ "Fuel consumption outside city (in l/km)",
                    variable == "fuel_type" ~ "Fuel type of the car",
                    variable == "gears" ~ "Number of gears of the car",
                    variable == "interior_color" ~ "Interior color of the car",
                    variable == "mileage" ~ "Mileage of the car (in km)",
                    variable == "model" ~ "Model of the car",
                    variable == "num_doors" ~ "Number of doors of the car",
                    variable == "num_previous_owners" ~ "Number of previous owners of the car",
                    variable == "ofid" ~ "Identifier for the listing",
                    variable == "power" ~ "Power of the car (in kW)",
                    variable == "price" ~ "Price of the car",
                    variable == "provider_type" ~ "Type of the provider",
                    variable == "seats" ~ "Number of seats of the car",
                    variable == "transmission" ~ "Transmission type of the car",
                    variable == "uniqueID_gen" ~ "Unique identifier (RWI)",
                    variable == "vat_deductible" ~ "Indicator for VAT being deductible",
                    variable == "vehicle_type" ~ "Type of the vehicle",
                    variable == "weight" ~ "Weight of the car (in kg)",
                    variable == "zipcode" ~ "Zip-code of the seller"
                )
            )

        # Check that no variable was not labeled
        targets::tar_assert_true(
            length(which(is.na(label_data$label))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not labeled.",
                " (Error code: cvl#2)"
            )
        )

        #--------------------------------------------------
        # create variable types

        label_data <- label_data |>
            dplyr::mutate(
                variable_type = NA_character_,
                variable_type = dplyr::case_when(
                    variable %in% c(
                        "price",
                        "mileage",
                        "power",
                        "co2_emissions",
                        "cylinders",
                        "displacement",
                        "fuel_consumption_city",
                        "fuel_consumption_rural",
                        "fuel_consumption_mixed",
                        "gears",
                        "num_doors",
                        "num_previous_owners",
                        "seats",
                        "weight",
                        "uniqueID_gen"
                    ) ~ "numeric",
                    variable %in% c(
                        "ofid",
                        "first_registration",
                        "city",
                        "zipcode",
                        "country_zip_code",
                        "country_code",
                        "created_date",
                        "carmkt_delivery",
                        "carmkt_version",
                        "brand",
                        "model",
                        "provider_type",
                        "body_color",
                        "equipment_first",
                        "interior_color",
                        "fuel_type",
                        "vehicle_type",
                        "efficiency_class",
                        "emission_class",
                        "emission_sticker",
                        "transmission"
                    ) ~ "character",
                    variable %in% c(
                        "asking_price",
                        "vat_deductible"
                    ) ~ "categorical",
                )
            )

        # Check that no variable was not labeled
        targets::tar_assert_true(
            length(which(is.na(label_data[["variable_type"]]))) == 0,
            msg = glue::glue(
                "!!! WARNING: ",
                "Some variables were not assigned.",
                " (Error code: cvl#3)"
            )
        )

        #--------------------------------------------------
        # export to excel

        openxlsx::write.xlsx(
            label_data,
            file.path(
                config_paths()[["output_path"]],
                config_globals()[["next_version"]],
                "info",
                "variable_labels.xlsx"
            ),
            rowNames = FALSE
        )

        #--------------------------------------------------
        # export to latex

        latex_table <- label_data |>
            dplyr::mutate(
                label = dplyr::case_when(
                    variable == "co2_emissions" ~ "CO$_2$ emissions of the car (in g/km)",
                    TRUE ~ label
                ),
                variable = stringr::str_replace_all(variable, "_", "\\\\_")
            ) |>
            dplyr::relocate(category) |>
            dplyr::arrange(category)

        # move identifies to the top of the table
        # move other features to the bottom of the table
        latex_table <- rbind(
            latex_table |> dplyr::filter(grepl("Identifiers", category)),
            latex_table |> dplyr::filter(!grepl("Identifiers|Other", category)),
            latex_table |> dplyr::filter(grepl("Other", category))
        )

        # write to latex
        latex_table |> kableExtra::kbl(
                escape = FALSE,
                format = "latex",
                longtable = TRUE,
                align = "l",
                linesep = "",
                caption = "List of Variables",
                col.names = c(
                    "Variable",
                    "\\makecell[l]{Variable\\\\name}",
                    "\\makecell[l]{Variable\\\\label}",
                    "\\makecell[l]{Variable\\\\type}"
                ),
                label = "list_variables"
            ) |>
            kableExtra::kable_styling(
                latex_options = c(
                    "striped",
                    "hold_position"
                ),
                # NOTE: color is defined in report latex file
                # see coding: \definecolor{user_gray}{rgb}{0.851,0.851,0.851}
                stripe_color = "user_gray"
            ) |>
            kableExtra::save_kable(
                file.path(
                    config_paths()[["output_path"]],
                    config_globals()[["next_version"]],
                    "info",
                    "variable_labels.tex"
                ),
                label = "tab:list_variables"
            )

    #--------------------------------------------------
    # return

    return(label_data)
}