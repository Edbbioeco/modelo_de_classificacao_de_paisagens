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
  dplyr::filter(name_muni %in% c("Abreu e Lima",
                                 "Araçoiaba",
                                 "Cabo de Santo Agostinho",
                                 "Camaragibe",
                                 "Igarassu",
                                 "Ipojuca",
                                 "Itamaracá",
                                 "Itapissuma",
                                 "Jaboatão dos Guararapes",
                                 "Moreno",
                                 "Olinda",
                                 "Paulista",
                                 "Recife",
                                 "São Lourenço da Mata"))

