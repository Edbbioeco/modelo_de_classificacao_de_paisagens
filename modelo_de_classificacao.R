# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(randomForest)

library(ggspatial)

library(ggview)

library(patchwork)

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

# Modelo de classificação ----

## Valores ----

extrair_rasters <- function(rasters){

  nome <- names(rasters)

  valores <- rasters |>
    terra::extract(pontos) |>
    dplyr::mutate(Class = pontos$Class |> as.factor()) |>
    dplyr::select(-ID)

  assign(paste("valores_", nome) |>
           stringr::str_remove_all(" "),
         valores,
         envir = globalenv())

}

rasters <- c("img_sat", "uso_cob", "ndvi") |>
  mget(envir = globalenv())

rasters

purrr::map(rasters, extrair_rasters)

ls(pattern = "valores_") |>
  mget(envir = globalenv())

## Criar os modelos ----

criar_modelos <- function(valores, id_raster){

  modelo <- randomForest::randomForest(Class ~.,
                                       data = valores,
                                       ntree = 1000)

  assign(paste0("modelo_", id_raster),
         modelo,
         envir = globalenv())

}

valores <- ls(pattern = "valores_") |>
  mget(envir = globalenv())

valores

id_raster <- c("uso_cob",
               "ndvi",
               "img_sat")

id_raster

purrr::map2(valores, id_raster, criar_modelos)

## Predições ----

criar_pred <- function(id){

  predicao <- terra::predict(rasters[[id]],
                             modelos[[id]],
                             na.rm = TRUE)

  assign(paste0("pred_", id_raster[id]),
         predicao,
         envir = globalenv())

}

modelos <- list(modelo_img_sat,
                modelo_uso_cob,
                modelo_ndvi)

modelos

id_raster <- c("uso_cob",
               "img_sat",
               "ndvi")

id_raster

id <- 1:3

id

purrr::map(id, criar_pred)

## Visualizar as predições ----

predicoes <- ls(pattern = "^pred_") |>
  mget(envir = globalenv())

predicoes

visualizar_pred <- function(predicoes){

  ggplot() +
    tidyterra::geom_spatraster(data = predicoes) +
    scale_fill_viridis_d(na.value = "transparent")

}

purrr::map(predicoes, visualizar_pred)

# Comparações ----

## Mapa da imagem de satélite ----

map_sat <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(-35.26594, -34.82002, 0.25)) +
  ggspatial::annotation_scale(location = "br",
                              text_cex = 2,
                              height = unit(0.5, "cm"),
                              bar_cols = c("black", "gold")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 7.5)

map_sat

## Mapa da Predicao ----

map_pred <- ggplot() +
  tidyterra::geom_spatraster(data = pred_img_sat |>
                               tidyterra::mutate(class = dplyr::case_when(
                                 class == "0" ~ "Matriz",
                                 class == "1" ~ "Fragmento"
                               ))) +
  scale_fill_manual(values = c("green4", "orange"),
                    na.translate = FALSE) +
  scale_x_continuous(breaks = seq(-35.26594, -34.82002, 0.25)) +
  labs(fill = NULL) +
  coord_sf(label_graticule = "ES") +
  ggspatial::annotation_scale(location = "br",
                              text_cex = 2,
                              height = unit(0.5, "cm"),
                              bar_cols = c("black", "gold")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text =  element_text(color = "black", size = 20)) +
  ggview::canvas(height = 10, width = 7.5)

map_pred

## Unir os mapas ----

(map_sat + map_pred) +
  ggview::canvas(height = 10, width = 16)

ggsave(filename = "mapa_predicao.png",
       height = 10, width = 16)
