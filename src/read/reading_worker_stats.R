reading_worker_stats <- function() {
    #' @title Reading worker stats
    #' 
    #' @description This function reads the worker metrics logs and exports the
    #' stats to a text file in a formatted way.
    #' 
    #' @return List with the latest files and the controller stats.
    #' @author Patrick Thiel

    #--------------------------------------------------
    # list all files in the worker_metrics folder
    # and get file information
    
    files <- file.info(list.files(
        file.path(
            config_paths()[["logs_path"]],
            "worker_metrics",
            "worker_metrics_history"
        ),
        pattern = ".log",
        full.names = TRUE
    ))

    files$file <- rownames(files)
    rownames(files) <- NULL

    #--------------------------------------------------
    # clean files

    files <- files |>
        dplyr::mutate(
            worker = stringr::str_extract(
                basename(file),
                "(worker_worker_[0-9])"
            )
        ) |>
        dplyr::relocate(c(file, worker), 1) |>
        dplyr::arrange(mtime)

    #--------------------------------------------------
    # get the latest file for each worker

    latest_files <- files |>
        dplyr::group_by(worker) |>
        dplyr::filter(mtime == max(mtime)) |>
        dplyr::ungroup() |>
        as.data.frame()

    #--------------------------------------------------
    # read the latest files logs for each worker

    controller_stats_list <- list()
    for (file_row in latest_files$file) {
        controller_file <- autometric::log_read(
            file_row,
            units_time = "seconds",
            units_memory = "megabytes",
            units_cpu = "percentage"
        ) |>
            dplyr::mutate(
                name = stringr::str_extract(
                    name,
                    "(worker_[0-9])"
                )
            ) |>
            dplyr::rename(
                worker = name,
                process_id = pid,
                memory_used_mb = resident
            ) |>
            dplyr::select(-c(version, virtual)) |>
            dplyr::filter(phase != "__DEFAULT__")

        # extract last execution time of worker
        # NOTE: because we select the latest file for each worker, it does not
        # mean that the last execution of the pipeline used this worker (only if
        # there is enough work to do)
        last_execution <- latest_files |>
            dplyr::filter(file == file_row) |>
            dplyr::select(last_execution = mtime) |>
            dplyr::pull()

        controller_file <- controller_file |>
            dplyr::mutate(
                last_execution = last_execution
            ) |>
            dplyr::relocate(last_execution, .after = worker)

        controller_stats_list[[file_row]] <- controller_file
    }

    # combine all worker stats
    controller_stats <- data.table::rbindlist(controller_stats_list)

    controller_stats <- controller_stats |>
        dplyr::arrange(worker, last_execution)

    #--------------------------------------------------
    # export stats

    w <- 30
    names(controller_stats) <- format(
        names(controller_stats),
        width = w,
        justify = "right"
    )

    gdata::write.fwf(
        controller_stats,
        file.path(
            config_paths()[["logs_path"]],
            "worker_metrics",
            "worker_stats.txt"
        ),
        justify = "right",
        rownames = FALSE,
        width = w
    )

    #--------------------------------------------------
    # combine output

    output <- list(
        "latest_files" = latest_files,
        "controller_stats" = controller_stats
    )

    #--------------------------------------------------
    # return

    return(output)
}