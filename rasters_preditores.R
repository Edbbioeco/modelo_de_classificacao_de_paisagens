# Pacotes ----

library(geobr)

library(tidyverse)

library(terra)

library(tidyterra)

library(sf)

library(maptiles)

library(appeears)

# Dados ----

## Shapefiles dos municípios da Região Metropolitana do Recife ----

### Importado ----

rmr <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni %in% c("Abreu E Lima",
                                 "Araçoiaba",
                                 "Cabo De Santo Agostinho",
                                 "Camaragibe",
                                 "Igarassu",
                                 "Ipojuca",
                                 "Ilha De Itamaracá",
                                 "Itapissuma",
                                 "Jaboatão Dos Guararapes",
                                 "Moreno",
                                 "Olinda",
                                 "Paulista",
                                 "Recife",
                                 "São Lourenço Da Mata") &
                  abbrev_state == "PE")

### Visualizando ----

rmr

ggplot() +
  geom_sf(data = rmr, color = "black")

# Rasters preditores ----

## Uso e cobertura do solo ----

### Importar ----

download.file("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif",
              "mapbiomas_2024_local.tif",
              mode = "wb")

uso_cob <- terra::rast("mapbiomas_2024_local.tif")

### Recortar ----

uso_cob <- uso_cob |>
  terra::crop(rmr) |>
  terra::mask(rmr)

### Visualizar ----

ggplot() +
  tidyterra::geom_spatraster(data = uso_cob) +
  scale_fill_viridis_c(na.value = "transparent")

## Imagem de satélite ----

### Importar ----

img_sat <- maptiles::get_tiles(x = rmr,
                               provider = "Esri.WorldImagery",
                               zoom = 14,
                               crop = TRUE)

### Recortar ----

img_sat <- img_sat |>
  terra::crop(rmr) |>
  terra::mask(rmr)

### Visualizar ----

img_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  scale_fill_viridis_c(na.value = "transparent")

## MDVI ----

### bbox ----

rmr_bbox <- rmr |>
  sf::st_bbox()

rmr_bbox

### Requisição ----

requisicao <- data.frame(task = "MCD43A4.061_Recife_30m",
                         subtask = "MCD43A4.061_Area",
                         product = "MCD43A4.061",
                         layer = c("Nadir_Reflectance_Band3",
                                   "Nadir_Reflectance_Band4"),
                         type = "area",
                         start = "2020-01-01",
                         end = "2026-01-01",
                         stringsAsFactors = FALSE,
                         latitude = mean(rmr_bbox[2], rmr_bbox[4]),
                         longitude = mean(rmr_bbox[1], rmr_bbox[3]))|>
  appeears::rs_build_task(roi = rmr |>
                            sf::st_bbox() |>
                            sf::st_as_sfc() |>
                            sf::st_as_sf(),
                          format = "geotiff")

requisicao

appeears::rs_request(request = requisicao,
                     user = "edsonbbioeco",
                     transfer = TRUE,
                     path = getwd(),
                     verbose = TRUE)
