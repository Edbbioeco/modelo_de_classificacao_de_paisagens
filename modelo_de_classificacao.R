# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(CDSE)

library(rsi)

library(terra)

library(tidyterra)

library(ranger)

library(DescTools)

library(ggspatial)

library(ggview)

library(patchwork)

# Dados ----

## Shapefile da região metropolitana de Recife ----

### Importado ----

rmr <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni %in% c("Abreu E Lima",
                                 "Araçoiaba",
                                 "Cabo de Santo Agostinho",
                                 "Camaragibe",
                                 "Igarassu",
                                 "Ipojuca",
                                 "Ilha de Itamaracá",
                                 "Itapissuma",
                                 "Jaboatão dos Guararapes",
                                 "Moreno",
                                 "Olinda",
                                 "Paulista",
                                 "Recife",
                                 "São Lourenço da Mata") &
                  abbrev_state == "PE")

### Visualizando ----

rmr

ggplot() +
  geom_sf(data = rmr, color = "black")

### Tratar ----

rmr_trart <- rmr |>
  sf::st_make_valid() |>
  dplyr::summarise(geometry = geometry |> sf::st_union()) |>
  nngeo::st_remove_holes()

rmr_trart

ggplot() +
  geom_sf(data = rmr_trart, color = "black")

## Shapefile de pontos ----

### Importar ----

pontos <- sf::st_read("pontos_rmr.gpkg")

### Tratar ----

pontos <- pontos |>
  dplyr::mutate(Class = Class |> as.factor())

### Visualizar ----

pontos

ggplot() +
  geom_sf(data = pontos,
          aes(fill = Class),
          size = 5,
          shape = 21,
          color = "black",
          stroke = 1) +
  scale_fill_viridis_d()

## Raster de imagem de satélite ----

### Autenticar usuário ----

cdse_client <- CDSE::GetOAuthClient(id = Sys.getenv("CDSE_ID"),
                                    secret = Sys.getenv("CDSE_SECRET"))

### Catálogo ----

catalogo <- CDSE::SearchCatalog(aoi = rmr_trart,
                                from = "2025-01-01",
                                to = "2026-11-01",
                                collection = "sentinel-2-l2a",
                                with_geometry = FALSE,
                                filter = "eo:cloud_cover < 5",
                                client = cdse_client)

catalogo

### Periodo ----

data <- catalogo |>
  dplyr::slice_min(tileCloudCover, n = 1) |>
  dplyr::pull(acquisitionDate)

data

## Evalscripts ----

### RGB ----

evalscript_rgb <- system.file("scripts",
                              "TrueColor.js",
                              package = "CDSE")

evalscript_rgb

### NDVI ----

evalscript_ndvi <- rsi::spectral_indices() |>
  dplyr::filter(short_name == "NDVI") |>
  CDSE::MakeEvalScript(constellation = "sentinel-2") |>
  paste(collapse = "\n")

evalscript_ndvi

### Baixar rasters ----

rasters <- purrr::pmap(
  list(evalscripts = ls(pattern = "^evalscript") |>
         mget(envir = globalenv())),
  \(evalscripts){

    CDSE::GetImage(bbox = rmr_trart |> sf::st_bbox(),
                   time_range = data,
                   script = evalscripts,
                   collection = "sentinel-2-l2a",
                   format = "image/tiff",
                   mosaicking_order = "leastRecent",
                   resolution = 50,
                   mask = TRUE,
                   buffer = 100,
                   client = cdse_client)

    },
  .progress = TRUE) |>
  terra::rast() |>
  terra::mask(rmr_trart) |>
  terra::crop(rmr_trart)

### Tratar os nomes dios rasters ----

rasters

names(rasters) <- c("NDVI", "R", "G", "B")

rasters

### Visualizar ----

rasters

ggplot() +
  tidyterra::geom_spatraster(data = rasters[[1]]) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                direction = -1,
                                na.value = "transparent")

ggplot() +
  tidyterra::geom_spatraster_rgb(data = rasters[[2:4]])

### Calcular índices ----

rasters <- c(rasters,
             vari = c(rasters$G - rasters$R) / (rasters$G + rasters$R - rasters$B),
             exg = 2 * rasters$G - rasters$R - rasters$B,
             sd_g_3 = terra::focal(rasters$G, w = 3, fun = "sd",
                                   na.rm = TRUE, expand = TRUE),
             sd_g_7 = terra::focal(rasters$G, w = 7, fun = "sd",
                                   na.rm = TRUE, expand = TRUE),
             sd_g_15 = terra::focal(rasters$G, w = 15, fun = "sd",
                                    na.rm = TRUE, expand = TRUE))

names(rasters) <- c("NDVI", "R", "G", "B",
                    "vari", "exg", "sd_g_3", "sd_g_7", "sd_g_15")

rasters$vari <- rasters$vari |>
  terra::subst(from = c(-Inf, Inf), to = NA) |>
  terra::clamp(lower = -1, upper = 1, values = FALSE)

rasters

rasters |> names()

ggplot() +
  tidyterra::geom_spatraster(data = rasters[[5]]) +
  scale_fill_distiller(palette = "RdYlGn",
                       direction = 1,
                       limits = c(-1, 1),
                       na.value = "transparent") +
  facet_wrap(~lyr)

ggplot() +
  tidyterra::geom_spatraster(data = rasters[[6:9]]) +
  scale_fill_distiller(palette = "RdYlGn",
                       direction = 1,
                       na.value = "transparent") +
  facet_wrap(~lyr)

# Random forest ----

## Extrair os dados ----

valores <- pontos |>
  dplyr::bind_cols(terra::extract(rasters,
                                  terra::vect(pontos),
                                  ID = FALSE)) |>
  tidyr::drop_na()

valores

## Distância de autocorrelação ----

ac <- blockCV::cv_spatial_autocor(r = rasters |>
                                    terra::project("EPSG:31985"),
                                  num_sample = 5000)

ac

## Validação cruzada ----

cv <- blockCV::cv_spatial(x = valores |>
                            dplyr::select(-c(1:2)),
                          column = "classe",
                          size = ac$range,
                          k = 5,
                          selection = "random",
                          iteration = 100)

cv

ggplot() +
  geom_sf(data = cv$blocks)

cv$blocks |>
  sf::st_transform(crs = 31985) |>
  sf::st_area()

## Métricas das variáveis ----

metricas <- cv$folds_list |>
  purrr::map_dfr(
    \(folder){

      treino <- valores[folder[[1]], ] |>
        sf::st_drop_geometry()

      teste <- valores[folder[[2]], ] |>
        sf::st_drop_geometry()

      rf <- ranger::ranger(Class ~ .,
                           data = treino,
                           num.trees = 500,
                           class.weights = 1 / as.vector(table(treino$Class)))

      pred <- stats::predict(rf, teste)$predictions

      tibble::tibble(acuracia = mean(pred == teste$Class))

    },
    .id = "fold",
    .progress = TRUE)

metricas

metricas |>
  dplyr::summarise(media = mean(acuracia),
                   sd = sd(acuracia))

## Modelo final ----

### Criar modelo ----

randomforest <- ranger::ranger(Class ~ .,
                               data = valores |>
                                 dplyr::select(-c(1:2)) |>
                                 sf::st_drop_geometry(),
                               num.trees = 500,
                               class.weights = 1 / as.vector(table(valores$Class)),
                               probability = TRUE,
                               importance = "permutation")

randomforest

### Importância das variáveis ----

randomforest |>
  ranger::importance() |>
  tibble::enframe(name = "Variável",
                  value = "Importância") |>
  dplyr::mutate(Variável = Variável |>
                  forcats::fct_reorder(Importância)) |>
  ggplot(aes(Importância, Variável)) +
  geom_col() +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1))

### Predição do raster ----

raster_pred <- terra::predict(rasters,
                              randomforest,
                              fun = \(modelo, dados, ...){

                                stats::predict(modelo, dados)$predictions[, "1"]

                                },
                              na.rm = TRUE)

raster_pred

ggplot() +
  tidyterra::geom_spatraster(data = raster_pred) +
  scale_fill_distiller(palette = "RdYlGn",
                       direction = 1,
                       limits = c(0, 1),
                       na.value = "transparent") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1))

### Mapa de classes ----

raster_pred_class <- raster_pred |>
  dplyr::mutate(lyr1 = dplyr::case_when(lyr1 >= 0.5 ~ "Floresta",
                                        lyr1 < 0.5 ~ "Matriz") |>
                  forcats::fct_relevel(c("Matriz", "Floresta")))

raster_pred_class

ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_class) +
  scale_fill_manual(values = c("orange", "forestgreen"),
                    na.value = "transparent") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1))

## Mapa ----

### Mapas das variáveis ----

mapa_var_ndvi <- ggplot() +
  tidyterra::geom_spatraster(data = rasters[[1]]) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                direction = -1,
                                na.value = "transparent",
                                guide = guide_colourbar(
                                  title = "NDVI",
                                  title.vjust = 0.5,
                                  barheight = 20,
                                  barwidth = 2.5,
                                  frame.colour = "black"
                                  )) +
  geom_sf(data = rmr_trart, color = "black", fill = "transparent", linewidth = 1) +
  labs(title = "NDVI") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        plot.title = element_text(color = "black", size = 30, hjust = 0.5),
        axis.line = element_line(color = "black", linewidth = 1))

mapa_var_ndvi

mapa_var_rgb <- ggplot() +
  tidyterra::geom_spatraster_rgb(data = rasters[[2:4]]) +
  geom_sf(data = rmr_trart, color = "black", fill = "transparent", linewidth = 1) +
  labs(title = "RGB") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        plot.title = element_text(color = "black", size = 30, hjust = 0.5),
        axis.line = element_line(color = "black", linewidth = 1))

mapa_var_rgb

mapa_var_vari <- ggplot() +
  tidyterra::geom_spatraster(data = rasters[[5]]) +
  scale_fill_distiller(palette = "RdYlGn",
                       direction = 1,
                       limits = c(-1, 1),
                       na.value = "transparent",
                       guide = guide_colourbar(
                         title = "VARI",
                         title.vjust = 0.5,
                         barheight = 20,
                         barwidth = 2.5,
                         frame.colour = "black"
                       )) +
  geom_sf(data = rmr_trart, color = "black", fill = "transparent", linewidth = 1) +
  labs(title = "VARI") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        plot.title = element_text(color = "black", size = 30, hjust = 0.5),
        axis.line = element_line(color = "black", linewidth = 1))

mapa_var_vari

mapa_var_sd <- ggplot() +
  tidyterra::geom_spatraster(data = rasters[[6:9]]) +
  tidyterra::scale_fill_hypso_c(palette = "colombia_hypso",
                                direction = -1,
                                na.value = "transparent",
                                guide = guide_colourbar(
                                  title = "Green sd",
                                  title.vjust = 0.5,
                                  barheight = 20,
                                  barwidth = 2.5,
                                  frame.colour = "black")) +
  geom_sf(data = rmr_trart, color = "black", fill = "transparent", linewidth = 1) +
  facet_wrap(~lyr) +
  labs(title = "Green SD") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        plot.title = element_text(color = "black", size = 30, hjust = 0.5),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", fill = "gray"),
        axis.line = element_line(color = "black", linewidth = 1))

mapa_var_sd

mapas_vars <- ls(pattern = "^mapa_var") |>
  mget(envir = globalenv()) |>
  patchwork::wrap_plots() +
  patchwork::plot_layout(ncol = 2) +
  patchwork::plot_annotation(title = "Preditores",
                             theme = theme(plot.title = element_text(color = "black",
                                                                     size = 30,
                                                                     hjust = 0.5)))

mapas_vars

### Mapa da predição ----

mapa_predicao <- ggplot() +
  tidyterra::geom_spatraster(data = raster_pred_class) +
  geom_sf(data = rmr_trart, color = "black", fill = "transparent", linewidth = 1) +
  scale_fill_manual(values = c("orange", "forestgreen"),
                    na.translate = FALSE,
                    name = "Classe") +
  labs(title = "Predição com Random Forest Espacial") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        axis.line = element_line(color = "black", linewidth = 1),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        plot.title =  element_text(color = "black", size = 30, hjust = 0.5))

mapa_predicao

### Unir os mapas ----

(mapas_vars | mapa_predicao) +
  patchwork::plot_layout(widths = c(2, 1))
