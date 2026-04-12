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

### Autenticar login ----

source("configurar_apppeears.R")

### lista de produtos ----

produtos <- appeears::rs_products()

produtos |>
  dplyr::pull(ProductAndVersion)

### Lista de layers ----

appeears::rs_layers(product = "HLSL30.020")

### bbox ----

rmr_bbox <- rmr |>
  sf::st_bbox()

rmr_bbox

### Requisição ----

requisicao <- data.frame(task = "polygon",
                         subtask = "US-Ha1",
                         product = "HLSL30.020",
                         layer = c("B04",
                                   "B05"),
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

### Importar ----

red <- terra::rast("HLSL30.020_B04_doy2025362_aid0001_25S.tif")

nir <- terra::rast("HLSL30.020_B05_doy2025362_aid0001_25S.tif")

### Calculo ----

ndvi <- ((nir * 0.0001) - (red * 0.0001)) / ((nir * 0.0001) + (red * 0.0001))

### Recortar ----

ndvi <- ndvi |>
  terra::crop(rmr) |>
  terra::mask(rmr)

### Ajuste de valores ----

names(ndvi) <- "ndvi"

ndvi <- ndvi |>
  tidyterra::filter(ndvi |> dplyr::between(-1, 1))

### Visualizar -----

ggplot() +
  tidyterra::geom_spatraster(data = ndvi) +
  scale_fill_viridis_c(na.value = "transparent",
                       limits = c(-1, 1))

# Exportando ----

## Imagem de Satélite ----

img_sat |>
  terra::writeRaster("img_sat.tif")

## Uso e cobertura do solo ----

uso_cob |>
  terra::writeRaster("uso_cob.tif")

## NDVI ----

ndvi |>
  terra::writeRaster("ndvi.tif")
