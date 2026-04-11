# Pacotes ----

library(geobr)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(sf)

library(terra)

library(mapedit)

# Dados ----

## Importado ----

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

## Visualizando ----

rmr

ggplot() +
  geom_sf(data = rmr, color = "black")

# Pontos ----

## Imagem de Satélite ----

mapa <- leaflet::leaflet() |>
  leaflet::addProviderTiles(providers$Esri.WorldImagery) |>
  leaflet.extras::addDrawToolbar(targetGroup = "draw",
                                 polylineOptions = TRUE,
                                 polygonOptions = TRUE,
                                 circleOptions = TRUE,
                                 rectangleOptions = TRUE,
                                 markerOptions = TRUE,
                                 editOptions = leaflet.extras::editToolbarOptions()) |>
  leaflet::addPolygons(data = rmr |>
                         dplyr::summarise(geom = geom |> sf::st_union()))


mapa

## Pontos em fragmentos ----

### Marcar pontos ----

pontos_frag <- mapa |>
  mapedit::editMap(targetGroup = "draw",
                   polylineOptions = TRUE,
                   polygonOptions = TRUE,
                   circleOptions = TRUE,
                   rectangleOptions = TRUE,
                   markerOptions = TRUE,
                   editOptions = leaflet.extras::editToolbarOptions())

