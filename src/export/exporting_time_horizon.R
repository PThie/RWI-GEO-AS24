exporting_time_horizon <- function(
    auto_data = NA
) {
    #' @title Exporting time horizon
    #' 
    #' @description This function exports information on the time horizon of the
    #' data delivery.
    #' 
    #' @param auto_data Dataframe with raw auto data
    #' 
    #' @return Dataframe with time horizon information
    #' @author Patrick Thiel

    #--------------------------------------------------
    # export information on time horizon of the data

    # define export path
    directory <- file.path(
        config_paths()[["output_path"]],
        config_globals()[["next_version"]],
        "info",
        "dataset_info.txt"
    )

    # avoid appending if the file already exists
    if (file.exists(directory)) {
        file.remove(directory)
    }

    # create information with time horizon
    data_info <- as.data.frame(
        cbind(
            `Date labels` = c("Start date:", "End date:"),
            Dates = c(
                as.character(min(auto_data$partition_date)),
                as.character(max(auto_data$partition_date))
            )
        )
    )

    # define header for export
    header <- glue::glue(
        "
        #--------------------------------------------------
        Infos for {config_globals()[['dataset_name']]} {config_globals()[['next_version']]}
        #--------------------------------------------------
        "
    )

    # export header
    write(
        header,
        directory,
        append = TRUE
    )

    # export dates
    gdata::write.fwf(
        data_info,
        directory,
        append = TRUE,
        rownames = FALSE,
        colnames = FALSE
    )

    #--------------------------------------------------
    # test that dataset is not empty

    targets::tar_assert_nonempty(
        data_info,
        msg = glue::glue(
            "!!! WARNING:",
            " Dataset is empty.",
            " (Error code: eth#1)"
        )
    )

    #--------------------------------------------------
    # return

    return(data_info)
}