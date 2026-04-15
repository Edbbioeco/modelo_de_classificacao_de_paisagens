# pacotes ----

library(terra)

library(tidyverse)

library(tidyterra)

library(sf)

# Raster classificado ----

## Importar ----

predicao <- terra::rast("predicao.tif")

## Visualizar ----

predicao

ggplot() +
  tidyterra::geom_spatraster(data = predicao) +
  scale_fill_viridis_d(na.translate = FALSE)

# Manchas ----

## Identificar as manchas ----

manchas <- predicao |>
  tidyterra::filter(class == "1")

## Visualizar ----

manchas

ggplot() +
  tidyterra::geom_spatraster(data = manchas) +
  scale_fill_viridis_d(na.translate = FALSE)

## Área ----

### Total do raster ----

predicao |> terra::expanse() / 1e6

### Das manchas ----

manchas |> terra::expanse() / 1e6

# Converter o raster um shapefile ----

## Converter ----

mancha_sf <- manchas |>
  terra::as.polygons() |>
  sf::st_as_sf()

## Visualizar ----

mancha_sf

ggplot() +
  geom_sf(data = mancha_sf |>
            sf::st_union())

## Exportar ----

mancha_sf |>
  sf::st_write("manchas.shp")
