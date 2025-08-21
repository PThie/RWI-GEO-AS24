moving_log_files <- function(files = NA) {

    #--------------------------------------------------
    # move original log files to history folder

    purrr::walk(files$file, function(file) {
        fs::file_copy(
            path = file,
            new_path = file.path(
                config_paths()[["logs_path"]],
                "worker_metrics",
                "worker_metrics_history",
                basename(file)
            )
        )
        fs::file_delete(file)
    })

    #--------------------------------------------------
    # return

    return(NULL)
}