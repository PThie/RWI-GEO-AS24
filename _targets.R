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
    library(kableExtra)
    library(qs)
    library(cli)
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
    #--------------------------------------------------
    # Reading raw data
	tar_file_read(
		auto_data_raw,
		file.path(
            config_paths()[["data_path"]],
            "raw",
            config_globals()[["current_delivery"]]
        ),
		reading_auto_data(!!.x)
	),
    tar_target(
        mapping_tables,
        reading_mapping_tables()
    ),
    #--------------------------------------------------
    # Column type info and test
    tar_fst(
        column_types_benchmark,
        exporting_column_infos(
            auto_data = auto_data_raw
        )
    ),
    tar_target(
        column_types_test,
        testing_consistent_variables(
            auto_data = auto_data_raw,
            column_types_benchmark = column_types_benchmark
        )
    ),
    #--------------------------------------------------
    # Cleaning steps
    tar_fst(
        auto_data_cleaned,
        cleaning_auto_data(
            auto_data_raw = auto_data_raw,
            municipalities = municipalities
        )
    ),
    tar_fst(
        auto_data_mapped,
        mapping_id_variables(
            auto_data = auto_data_cleaned,
            mapping_tables = mapping_tables
        )
    ),
    tar_target(
        value_labels,
        exporting_value_labels(
            auto_data = auto_data_mapped
        )
    ),
    tar_fst(
        auto_data_renamed,
        cleaning_variable_names(
            auto_data = auto_data_mapped
        )
    )
)

#--------------------------------------------------
# Export

targets_export <- rlang::list2(
    tar_fst(
        auto_data_exported,
        exporting_auto_data(
            auto_data = auto_data_renamed
        )
    )
)

#--------------------------------------------------
# Unit test

targets_unit_testing <- rlang::list2(
    tar_eval(
        list(
            #--------------------------------------------------
            # Reading the exported SUF data
            tar_file_read(
                suf_exported_data,
                file.path(
                    config_paths()[["data_path"]],
                    "SUF",
                    config_globals()[["next_version"]],
                    exported_file_formats,
                    paste0(
                        "CARMKT_",
                        config_globals()[["next_version"]],
                        "_SUF.",
                        exported_file_formats
                    )
                ),
                reading_exported_data(
                    data_path = !!.x,
                    file_format = exported_file_formats
                )
            ),
            #--------------------------------------------------
            # Test whether all variables have been deleted that are supposed to
            # be deleted
            tar_target(
                deleted_variables_test,
                testing_deleted_variables(
                    auto_data = suf_exported_data,
                    file_format = exported_file_formats
                )
            )
        ),
        values = list(
            suf_exported_data = rlang::syms(helpers_target_names()[["suf_exported_data"]]),
            exported_file_formats = helpers_target_names()[["exported_file_formats"]],
            deleted_variables_test = rlang::syms(helpers_target_names()[["deleted_variables_test"]])
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
    targets_geo_data,
	targets_preparation_folders,
    targets_preparation_auto_data,
    targets_export,
    targets_unit_testing,
    targets_pipeline_stats
)