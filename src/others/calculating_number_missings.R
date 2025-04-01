calculating_number_missings <- function(
    auto_data = NA
) {
    #' @title Calculating the number of missings
    #' 
    #' @description This function calculates the number of missings of each
    #' variable for each missing type after the cleaning process.
    #' 
    #' @param auto_data Dataframe with cleaned auto data
    #' 
    #' @return Dataframe with missings
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # count missings
    
    calulating_missings_count <- function(var) {
        # calculate number of missings for each missing type
        missing_count <- c()
        for (missing in helpers_missing_values()) {
            missing_count <- c(
                missing_count,
                length(which(
                    auto_data[[var]] == missing
                ))
            )
        }

        # construct output dataframe
        output <- data.frame(
            variable = var,
            missing_type = names(helpers_missing_values()),
            missing_count = missing_count,
            missing_perc = missing_count / nrow(auto_data) * 100
        ) |>
        # drop all missings (because this is a vector, it will be not be calculated
        # correctly)
        dplyr::filter(missing_type != "all_missings")

        return(output)
    }
    
    missings_list <- list()
    for (var in colnames(auto_data)) {
        missings_list[[var]] <- calulating_missings_count(var)
    }

    missings <- data.table::rbindlist(missings_list)

    #--------------------------------------------------
    # export

    openxlsx::write.xlsx(
        missings,
        file.path(
            config_paths()[["output_path"]],
            config_globals()[["next_version"]],
            "info",
            "number_of_missings.xlsx"
        ),
        overwrite = TRUE
    )

    #--------------------------------------------------
    # return

    return(missings)
}