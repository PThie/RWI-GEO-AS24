#--------------------------------------------------
# description

# This file is solely responsible for generating some output for a news
# article regarding the publication the first version of the data.

#--------------------------------------------------
# libraries

library(ggplot2)

#--------------------------------------------------
# read data

auto_data <- tar_read(auto_data_cleaned_appended)

# matching data between zip-codes and municipalities
zip_munic <- data.table::fread(
    file.path(
        config_paths()[["gebiete_path"]],
        "Zuordnung",
        "zuordnung_r1_plz_gem_krs_2019.csv"
    )
)

# geographical data for municipalities
munic_geo <- sf::st_read(
    file.path(
        config_paths()[["gebiete_path"]],
        "Gemeinde",
        "2019",
        "VG250_GEM.shp"
    ),
    quiet = TRUE
)

district_geo <- sf::st_read(
    file.path(
        config_paths()[["gebiete_path"]],
        "Kreis",
        "2019",
        "VG250_KRS.shp"
    ),
    quiet = TRUE
)

#--------------------------------------------------
# add created year

auto_data <- auto_data |>
    dplyr::mutate(
        created_year = as.numeric(substring(created_date, 1, 4))
    )

#--------------------------------------------------
# clean zip-codes to munic data

zip_munic_clean <- zip_munic |>
    dplyr::select(PLZ, AGS_gem, AGS_kreis) |>
    dplyr::mutate(
        PLZ = as.character(PLZ),
        AGS_gem = as.character(AGS_gem),
        AGS_kreis = as.character(AGS_kreis),
        AGS_gem = stringr::str_pad(AGS_gem, 8, pad = "0"),
        AGS_kreis = stringr::str_pad(AGS_kreis, 5, pad = "0")
    ) |>
    dplyr::distinct(PLZ, .keep_all = TRUE)

#--------------------------------------------------
# subset to recent years
# for faster data processing + most information is available for recent years

auto_data_recent <- auto_data |>
    dplyr::filter(created_year >= 2023)

#--------------------------------------------------
# add zip-code to munic data

auto_data_merged <- merge(
    auto_data,
    zip_munic_clean,
    by.x = "zipcode",
    by.y = "PLZ",
    all.x = TRUE
)

#--------------------------------------------------
# calculate the share of offered electric cars

electric_share <- auto_data_merged |>
    dplyr::mutate(
        electric_share = dplyr::case_when(
            fuel_type == "Electric" ~ 1,
            TRUE ~ 0
        )
    ) |>
    dplyr::group_by(created_year) |>
    dplyr::summarise(
        electric_share = sum(electric_share) / dplyr::n()
    )

# calculate change in electric share
# electric_share <- electric_share |>
#     dplyr::group_by(AGS_kreis) |>
#     dplyr::mutate(
#         electric_share_change = electric_share - dplyr::lag(electric_share, 1)
#     )

#--------------------------------------------------
# generate bar plot

electric_share_plot <- ggplot2::ggplot(
    electric_share |> dplyr::filter(created_year >= 2019),
    ggplot2::aes(
        x = created_year,
        y = electric_share * 100
    )
) +
    scale_y_continuous(limits = c(0, 100))+
    geom_text(
        aes(label = scales::percent(electric_share, accuracy = 0.1)),
        vjust = -0.5,
        size = 7
    ) +
    ggplot2::geom_col(fill = "#0f5e86") +
    ggplot2::labs(
        title = "Share of Electric Cars Offered",
        x = "Year",
        y = "Share of Electric Cars"
    ) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
    )

ggsave(
    plot = electric_share_plot,
    file.path(
        config_paths()[["output_path"]],
        "miscellaneous",
        "electric_share_plot.png"
    ),
    dpi = 400
)




length(which(auto_data$zipcode == "04109"))
head(zip_munic_clean)

zip_munic_clean |> dplyr::filter(PLZ == "04109")

tst = zip_munic_clean |>
    dplyr::mutate(start = substring(PLZ, 1, 2))

tst |> dplyr::filter(start == "04")
sort(unique(tst$start))



tst2 = zip_munic |>
    dplyr::mutate(start = substring(PLZ, 1, 2))

sort(unique(tst2$start))
nrow(tst)


tst3 <- auto_data |>
    dplyr::mutate(start = substring(zipcode, 1, 2))

setdiff(unique(tst3$start), unique(tst2$start))



