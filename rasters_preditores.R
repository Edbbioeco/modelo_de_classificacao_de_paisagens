# Pacotes ----

library(geobr)

library(tidyverse)

library(terra)

library(tidyterra)

library(sf)

library(maptiles)

library(rgee)

# Dados ----

## Shapefiles dos municípios da Região Metropolitana do Recife

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
