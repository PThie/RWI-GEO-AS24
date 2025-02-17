#----------------------------------------------
# description

# This file is the main file that orchestrates the other coding files. It
# controls the data pipeline and defines the global settings.

###################################################
# PIPELINE SETTINGS
###################################################

#----------------------------------------------
# load libraries

suppressPackageStartupMessages({
    library(targets)
    library(renv)
    library(dplyr)
    library(here)
    library(tarchetypes)
    library(stringr)
    library(stringi)
    library(fst)
    library(crew)
    library(data.table)
    library(glue)
    library(purrr)
})

#----------------------------------------------
# set working directory

setwd(here::here())

#--------------------------------------------------
# Pipeline settings

# target options
tar_option_set(
    resources = tar_resources(
        fst = tar_resources_fst(compress = 50)
    ),
    seed = 1,
    garbage_collection = TRUE,
    memory = "transient",
    controller = crew_controller_local(
        name = "my_controller",
        workers = 3,
        seconds_idle = 10
    ),
    retrieval = "worker",
    storage = "worker"
)

#----------------------------------------------
# load configurations

source(
    file.path(
        here::here(),
        "src",
        "helpers",
        "config.R"
    )
)

#----------------------------------------------
# load R scripts

sub_directories <- list.dirs(
    config_paths()[["code_path"]],
    full.names = FALSE,
    recursive = FALSE
)

for (sub_directory in sub_directories) {
    if (sub_directory != "helpers") { 
        lapply(
            list.files(
                file.path(
                    config_paths()[["code_path"]],
                    sub_directory
                ),
                pattern = "\\.R$",
                full.names = TRUE,
                ignore.case = TRUE
            ),
            source
        )
    } else {
        files <- list.files(
            file.path(
                config_paths()[["code_path"]],
                sub_directory
            ),
            pattern = "\\.R$",
            full.names = TRUE,
            ignore.case = TRUE
        )
        files <- files[
            stringr::str_detect(
                files,
                "config.R$"
            ) == FALSE
        ]
        lapply(files, source)
    }
}

###################################################
# ACTUAL PIPELINE
###################################################

#--------------------------------------------------
# Folder generation

targets_preparation_folders <- rlang::list2(
	# Creating empty folders for upcoming version
	tar_target(
		empty_folders,
		creating_folder_structure()
	)
)

#--------------------------------------------------
# Geo data

targets_geo_data <- rlang::list2(
    tar_qs(
        municipalities,
        reading_municipalities(
            year_of_definition = 2023
        )
    )
)

#--------------------------------------------------
# Preparation AS data

targets_preparation_auto_data <- rlang::list2(
	tar_file_read(
		auto_data_raw,
		file.path(
            config_paths()[["data_path"]],
            "raw",
            config_globals()[["current_delivery"]]
        ),
		reading_auto_data(!!.x)
	),
    tar_fst(
        auto_data_cleaned,
        cleaning_auto_data(
            auto_data_raw = auto_data_raw
        )
    ),
    tar_fst(
        auto_data_renamed,
        cleaning_variable_names(
            auto_data = auto_data_cleaned
        )
    )
)

#--------------------------------------------------
# Pipeline stats

targets_pipeline_stats <- rlang::list2(
	tar_file(
		pipeline_stats,
		helpers_monitoring_pipeline(),
		cue = tar_cue(mode = "always")
	)
)

#--------------------------------------------------
# combine all

rlang::list2(
	targets_preparation_folders,
	targets_pipeline_stats,
    targets_preparation_auto_data
)