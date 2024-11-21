helpers_monitoring_pipeline <- function() {
    #' @title Monitor pipeline
    #' 
    #' @description This function monitors the pipeline and writes the results
    #' to a log file.
    #' 
    #' @return NULL
    #' @author Patrick Thiel
    
    #--------------------------------------------------
    # pipeline stats

    pipeline_stats <- tar_progress_summary() |>
        as.data.frame()

    gdata::write.fwf(
        pipeline_stats,
        file.path(
            config_paths()[["logs_path"]],
            "pipeline_stats.txt"
        ),
        rownames = FALSE
    )

    #--------------------------------------------------
    # worker stats
    
    worker_stats <- tar_crew() |>
        as.data.frame()
        
    gdata::write.fwf(
        worker_stats,
        file.path(
            config_paths()[["logs_path"]],
            "worker_stats.txt"
        ),
        rownames = FALSE
    )

    #--------------------------------------------------
    # return
    # NOTE: return can only be a character since the output type has to align
    # with tar_file (original idea was to return a list with the two dataframes)
    # NOTE: targets type has to be tar_file in order to use tar_progress_summary
    # and tar_crew within the pipeline

    return(NULL)
}