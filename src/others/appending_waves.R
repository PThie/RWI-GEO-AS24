appending_waves <- function(
    deliveries = NA,
    dependency = NA
) {
    #' @title Combining deliveries
    #' 
    #' @description This function combines all deliveries into one dataset.
    #' 
    #' @param deliveries List with all deliveries
    #' @param dependency Object from previous step to indicate dependency between
    #' functions/ steps
    #' 
    #' @return Dataframe, combined data set
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # actively call dependency to make sure that this function is executed if
    # something changes upstream

    targets::tar_assert_nonempty(
        dependency,
        msg = glue::glue(
            "The dependency object is empty.",
            " (Error code: aw#1)"
        )
    )

    #--------------------------------------------------
    # read all data

    data_storage <- list()
    for (del in deliveries) {
        dta <- arrow::read_parquet(
            file.path(
                config_paths()[["data_path"]],
                "processed",
                paste0("Lieferung_", del),
                "clean_data.parquet"
            )
        )

        data_storage[[del]] <- dta
    }

    # append all data
    dta_append <- data.table::rbindlist(data_storage, fill = TRUE)

    #--------------------------------------------------
    # return

    return(dta_append)
}