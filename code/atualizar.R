library(tidyverse)
library(readxl)
library(nexo.utils)
library(sf)

#-- remover duplicados
popMunic %>%
  distinct() -> popMunic2

table(popMunic2$year)

#-- atualizar 2020

pop2020 <- readxl::read_excel('raw/tabela6579 (1).xlsx', skip=2) %>%
  drop_na() %>%
  set_names('ibge7', 'munic', 'pop') %>%
  mutate(pop = as.numeric(pop),
         ibge7 = as.numeric(ibge7),
         ibge6 = as.numeric(str_sub(ibge7,1,6)),
         year = 2020,
         ibge2 = as.numeric(str_sub(ibge7,1,2)),
         uf = str_sub(munic, -3,-2)) %>%
  select(uf, ibge2, ibge7, ibge6, year, pop)


popMunic2 %>%
  bind_rows(pop2020) -> popMunic

usethis::use_data(popMunic, overwrite = TRUE)

nexo.utils::popMunic %>%
  mutate(type = case_when(year <= 1980 ~ "Censo",
                          year %in% c(1991,2000,2010) ~ "Census",
                          TRUE ~ "Estimated")) -> popMunic

usethis::use_data(popMunic, overwrite = TRUE)

#-- info partidos
infoPartidos %>%
  set_names('party', 'abbrev2020', 'abbrev2016', 'number', 'hex') %>%
  drop_na(number) %>%
  add_row(party="Partido Pátria Livre",
          abbrev2020 = NA,
          abbrev2016 = "PPL",
          number = 54,
          hex = "#CB8C91") %>%
  mutate(active = ifelse(number %in% c(31,54,44), FALSE, TRUE)) -> infoParty

infoParty %>%
  mutate(hex = str_to_upper(hex)) -> infoParty

usethis::use_data(infoParty, overwrite = TRUE)

#-- mapState
mapState %>%
  left_join(nexo.utils::infoState[,c(1:3)], by=c('ibge2')) -> mapState

usethis::use_data(mapState, overwrite = TRUE)


#-- atualizar infoMunic
nexo.utils::infoMunic %>%
  group_by(ibge7) %>%
  mutate(n=n())  %>%
  filter(n>1)-> x2 %>%

nexo.utils::infoMunic %>%
  filter(muni!="MURICI" | (muni=="MURICI" & metropolitan == "RM Maceió")) -> x2

x2 %>%
  mutate(is_metro = ifelse(is.na(metropolitan), TRUE, FALSE),
         metro = metropolitan,
         is_capital = ifelse(capital==1, TRUE, FALSE)) %>%
  select(-one_of('metropolitan', 'capital'))  -> infoMunic

View(infoMunic)

nexo.utils::infoMunic %>%
  mutate(is_metro = ifelse(is_metro, FALSE, TRUE),
         is_capital = replace_na(is_capital, FALSE)) -> infoMunic

usethis::use_data(infoMunic, overwrite = TRUE)


# Info state e populacao --------------------------------------------------

nexo.utils::infoState %>%
  select(-one_of("pop")) -> infoState

usethis::use_data(infoState, overwrite = TRUE)

nexo.utils::popMunic %>%
  group_by(uf, ibge2, year) %>%
  summarise(pop = sum(pop, na.rm=T)) -> popState

usethis::use_data(popState, overwrite = TRUE)

popState %>%
  filter(str_sub(ibge2,1,1)==5) %>%
  ggplot(aes(x=year, y=pop, col=uf)) + geom_line() +
  scale_y_continuous(labels=scales::comma)


# Corrigir mapa ------------------------------------------------------------

library(sf)

st_crs(nexo.utils::mapMunic)
st_crs(nexo.utils::mapState)

# mapCountries <- st_read("mapCountries2/mapCountries2.shp")

mapCountries <- st_crs(nexo.utils::mapMunic)

usethis::use_data(mapCountries, overwrite = TRUE)

