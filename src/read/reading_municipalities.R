reading_municipalities <- function(year_of_definition = NA) {
    #' @title Read municipalities shapes
    #' 
    #' @description This function reads the municipalities shapes from the
    #' Gebietseinheit folder.
    #' 
    #' @param year_of_definition Year of the definition of the municipalities
    #' 
    #' @return Spatial dataframe with municipalities shapes
    #' @author Patrick Thiel

    #--------------------------------------------------
    # read municipalities

    municipalities <- sf::st_read(
        file.path(
            config_paths()[["gebiete_path"]],
            "Gemeinde",
            year_of_definition,
            "VG250_GEM.shp"
        )
    ) |>
        sf::st_transform(config_globals()[["utmcrs"]]) |>
        dplyr::select(
            ags = AGS,
            name = GEN,
            geometry
        ) |>
        dplyr::mutate(
            name = stringi::stri_trans_general(
                name,
                "de-ASCII; Latin-ASCII"
            )
        )

    #--------------------------------------------------
    # return

    return(municipalities)
}