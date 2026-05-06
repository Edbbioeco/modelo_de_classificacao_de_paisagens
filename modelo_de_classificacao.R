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

valores <- img_sat |>
    terra::extract(pontos) |>
    dplyr::mutate(Class = pontos$Class |> as.factor()) |>
    dplyr::select(-ID)

valores

## Criar os modelos ----

id <- 1:500

modelos <- purrr::map(id, ~ randomForest::randomForest(Class ~.,
                                                       data = valores,
                                                       ntree = 1000))

names(modelos) <- paste0("modelo_", id)

modelos

## Avaliando o modelo ----

purrr::imap(modelos, \(modelo, id){

  modelo$err.rate |>
    tibble::as_tibble() |>
    dplyr::mutate(modelo = id,
                  `N-Trees` = dplyr::row_number())

  }) |>
  dplyr::bind_rows() |>
  tidyr::pivot_longer(cols = 1:3,
                      names_to = "Error type",
                      values_to = "Error") |>
  dplyr::group_by(modelo, `Error type`, `N-Trees`) |>
  dplyr::summarise(media = Error |> mean(),
                   min = Error |> min(),
                   max = Error |> max(),
                   .groups = "drop") |>
  ggplot(aes(`N-Trees`, media, color = `Error type`, fill = `Error type`)) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.25) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_fill_manual(values = c("0" = "orange",
                               "1" = "forestgreen",
                               "OOB" = "black")) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Refazendo o modelo para apenas 200 árvores ----

modelo_ref <- randomForest::randomForest(Class ~.,
                                     data = valores,
                                     ntree = 200)

modelo_ref

## Predições ----

predicao <- terra::predict(img_sat,
                           modelo_ref,
                           na.rm = TRUE)

## Visualizar as predições ----

predicao

ggplot() +
  tidyterra::geom_spatraster(data = predicao) +
  scale_fill_viridis_d(na.translate = FALSE)

## Exportar a predição ----

predicao |>
  terra::writeRaster("predicao.tif",
                     overwrite = TRUE)

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
  tidyterra::geom_spatraster(data = predicao |>
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
