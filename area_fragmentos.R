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
