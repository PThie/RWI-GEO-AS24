cleaning_append_data <- function(
    auto_data = NA
) {
    #' @title Cleaning appended data
    #' 
    #' @description This function cleans the appended data.
    #' 
    #' @param auto_data Dataframe with appended auto data
    #' 
    #' @return Dataframe with cleaned appended data
    #' @author Patrick Thiel

    #--------------------------------------------------
    # drop complete duplicates

    auto_data_prep <- auto_data |>
        dplyr::distinct(.keep_all = TRUE)

    #--------------------------------------------------
    # generate unique ID (counting ID)

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            uniqueID_gen = seq(1, dplyr::n())
        )

    #--------------------------------------------------
    # create version the observations belong to

    auto_data_prep <- auto_data_prep |>
        dplyr::mutate(
            version = config_globals()[["next_version"]]
        )

    #--------------------------------------------------
    # return

    return(auto_data_prep)
}