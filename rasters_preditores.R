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

requisicao <- list(name = "ASTER_15m_rmr",
                   params = list(dates = list(list(start = "2020-01-01",
                                                   end = "2026-01-01")),
                                 layers = list(list(list(product = "AST_L1T.003",
                                                         layer = "VNIR_Band2"),
                                                    list(product = "AST_L1T.003",
                                                         layer = "VNIR_Band3N"))),
                                 coordinates = list(list(id = "RMR_Area",
                                                         box = rmr_bbox))),
                   format = list(type = "geotiff"))

requisicao

appeears::rs_transfer(requisicao)
