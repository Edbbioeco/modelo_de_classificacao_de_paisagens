# Pacotes ----

library(geobr)

library(tidyverse)

library(leaflet)

library(leaflet.extras)

library(terra)

library(mapedit)

library(sf)

# Dados ----

## Região metropolitana de Recife ----

### Importando ----

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
