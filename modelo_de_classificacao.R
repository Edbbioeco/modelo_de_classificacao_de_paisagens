# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(randomForest)

# Dados ----

## Shapefile de pontos ----

### Importar ----

pontos <- sf::st_read("pontos_rmr.shp")

### Tratar ----

pontos <- pontos |>
  dplyr::mutate(Class = Class |> as.factor())

### Visualizar ----

pontos

ggplot() +
  geom_sf(data = pontos, aes(color = Class)) +
  scale_color_viridis_d()

## Raster de imagem de satélite ----

### Importar ----

img_sat <- terra::rast("img_sat.tif")

### Visualizar ----

img_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = pontos, aes(color = Class)) +
  scale_color_viridis_d()

## Uso e cobertura do solo ----

### Importar ----

uso_cob <- terra::rast("uso_cob.tif")

### Visualizar ----

uso_cob

ggplot() +
  tidyterra::geom_spatraster(data = uso_cob) +
  scale_fill_continuous(na.value = "transparent") +
  geom_sf(data = pontos, aes(color = Class)) +
  scale_color_viridis_d()

## NDVI ----

### Importar ----

ndvi <- terra::rast("ndvi.tif")

### Visualizar ----

ndvi

ggplot() +
  tidyterra::geom_spatraster(data = ndvi) +
  scale_fill_continuous(na.value = "transparent") +
  geom_sf(data = pontos, aes(color = Class)) +
  scale_color_viridis_d()
