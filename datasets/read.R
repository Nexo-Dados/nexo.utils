library(tidyverse)
library(sf)

coordenadas %>%
  set_names('ibge7', 'ibge6', 'muni',
            'uf', 'lon', 'lat', 'metropolitan', 'capital') -> c

c %>% select(ibge7, ibge6) -> c2
municipios <- read_csv("datasets/data/municipios.csv") %>%
  select(1,6,3,4,5) %>%
  set_names("ibge7", "munic", "TSE", "RF", "BCB")

municipios %>%
  left_join(c, by=c('ibge7')) %>%
  select(7,8,1,6,2:5,9,10,11,12) -> m1


m2 <- read_csv("datasets/data/populacao_municipios2019.csv") %>%
  select(2,3,4,5,6) %>%
  set_names("pop", "year", "ibge6", "uf", "ibge2") %>%
  left_join(c2, by=c("ibge6")) %>%
  select(4,5,6,3,2,1)


save(m1, file= './data/infoMunic.Rdata')
save(m2, file= './data/popMunic.Rdata')
save(m3, file= './data/mapMunic.Rdata')

#-----

#--- ufs
library(readxl)
UF <- read_excel("datasets/ESPECIAL ABCP - estados.xlsx")
STATE <- st_read("datasets/maps/simple_state/UFEBRASIL.shp") %>%
  mutate(ibge2 = as.numeric(CD_GEOCODU)) %>%
  select(4,5)
GOV <- read_excel("datasets/data/governadores.xls") %>%
  left_join(UF, by=c("uf")) %>%
  select(uf, ibge2, elect_party, year)


save(UF, file= './data/infoState.Rdata')
save(STATE, file= './data/mapState.Rdata')
save(GOV, file= './data/govState.Rdata')

#---
Countries_2_51 <- read_csv("datasets/data/countries/Countries_2_5.csv") %>%
  select(-one_of("id")) %>%
  select(pt, region, wdb_code, alpha_3, continent5) %>%
  rename(iso3 = alpha_3)

Countries_2_5 <- read_csv("datasets/data/countries/Countries_2_5.csv") %>%
  select(-one_of("id")) %>%
  rename(iso3 = alpha_3,
         isoName = ISO_Name,
         iso2 = alpha_2,
         numCode = Numeric_code,
         countryLat = ponto_lat,
         countryLon = ponto_lon,
         isSmall = is_small,
         namePTBR = pt,
         isCountry = pais,
         parentState = parent_state,
         isEU =  is_eu,
         countryName = country_name,
         capitalName = capital_name,
         capitalLat = capital_latitude,
         capitalLon = capital_longitude)

save(Countries_2_51, file="./data/simpleCountries.Rdata")
save(Countries_2_5, file="./data/infoCountries.Rdata")



world_population <- read_csv("datasets/data/world/world_population.csv")

world_population %>%
  gather(year, pop, `1960`:`2017`) %>%
  select(WDB_code, year, pop) -> world

save(world, file="./data/popCountries.Rdata")

simple_map %>%
  rename(iso3 = alpha_3 ) -> s

save(s, file="./data/mapCountries.Rdata")




