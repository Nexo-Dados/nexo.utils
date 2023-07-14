
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

st_crs(mapCountries) <- st_crs(nexo.utils::mapMunic)

usethis::use_data(mapCountries, overwrite = TRUE)


# Pop 2021 ----------------------------------------------------------------

library(tidyverse)
library(readxl)

pop2021 <- read_excel("raw/estimativa_dou_2021.xls",
                      sheet = "Municípios", skip = 1) %>%
  set_names("uf", "ibge2", "ibge5", "nome", "pop") %>%
  mutate(year=2021,
         ibge7 = as.numeric(paste0(ibge2,ibge5)),
         type = "Estimated",
         ibge6 = as.numeric(str_sub(ibge7,1,6)),
         pop = as.numeric(gsub("\\.", "", gsub("\\(.*", "", pop)))) %>%
  select(uf, ibge2, ibge6, ibge7, year, pop, type) %>%
  drop_na()

popMunic %>%
  select(uf, ibge2, ibge6, ibge7, year, pop, type) %>%
  bind_rows(pop2021) -> popMunic

usethis::use_data(popMunic, overwrite = TRUE)

popMunic %>%
  group_by(uf, ibge2, year) %>%
  summarise(pop = sum(pop, na.rm=T)) -> popState

usethis::use_data(popState, overwrite = TRUE)

#-- mollweide no mapa
library(tidyverse)
library(sf)

mapCountries <- nexo.utils::mapCountries %>%
  st_transform(crs=st_crs("+proj=moll +x_0=0 +y_0=0 +lat_0=0 +lon_0=0"))


usethis::use_data(mapCountries, overwrite = TRUE)

##-- fao stat

library(readxl)
library(tidyverse)
library(sf)

fao_code <- read_excel("raw/fao_code.xlsx")

nexo.utils::infoCountries %>%
  left_join(fao_code[,c(2,3)], by=c('iso3'='ISO3'))  %>%
  mutate(isCountry = ifelse(isCountry==1, TRUE, FALSE),
         isSmall = ifelse(isSmall=="Yes", TRUE, FALSE),
         isEU = ifelse(isEU=="Yes", TRUE, FALSE)) -> infoCountries

usethis::use_data(infoCountries, overwrite = TRUE)


# Atualizar info countries ------------------------------------------------

library(tidyverse)

infoCountries <- readxl::read_excel("raw/countries.xlsx") %>%
  select(-region) %>%
  mutate_at(vars(21:24), round, digits=2) %>%
  mutate(isIndependent = ifelse(isIndependent=="VERDADEIRO", TRUE, FALSE),
         isEU = ifelse(isEU=="VERDADEIRO", TRUE, FALSE),
         isUN = ifelse(isUN=="VERDADEIRO", TRUE, FALSE),
         parentState = ifelse(parentState=="NA", NA, parentState)) %>%
  rename(wasUSSR = wasURSS)

usethis::use_data(infoCountries, overwrite = TRUE)

# Maps --------------------------------------------------------------------

mapCountries <- readRDS("./raw/mapCountries.rds")

usethis::use_data(mapCountries, overwrite = TRUE)

# Atualizar censo 2022 ----------------------------------------------------

#--
library(tidyverse)
library(readxl)

#-- sistematizacao censo
pop1 <- read_excel("./raw/ipeadata[18-01-2023-12-39].xls") %>%
  pivot_longer(cols = `1872`:`2010`) %>%
  drop_na(value) %>%
  set_names("uf", "codigo_ibge7", "municipio", "ano", "populacao") %>%
  mutate(ano = as.numeric(ano),
         codigo_ibge7 = as.numeric(codigo_ibge7))


pop2 <- read_xlsx("./raw/tabela4709.xlsx", skip=3) %>%
  select(1,3) %>%
  set_names("codigo_ibge7", "populacao") %>%
  mutate_at(vars(codigo_ibge7), as.numeric) %>%
  mutate(ano = 2022) %>%
  drop_na()

pop1 %>%
  bind_rows(pop2) %>%
  arrange(ano) %>%
  rename(ibge7=codigo_ibge7) %>%
  group_by(ibge7) %>%
  fill(uf, municipio) %>%
  drop_na() -> x

#-- pop munic
popMunic %>%
  select(uf, ibge2, ibge7, year, pop, type) %>%
  filter(!(year %in% c(1996,2007))) %>%
  filter(type!="Estimated") %>%
  mutate(type=ifelse(type=="Censo", "Census", type)) %>%
  bind_rows(x  %>%
              select(uf, ibge7, ano, populacao) %>%
              rename(year = ano, pop=populacao) %>%
              mutate(ibge2 = as.numeric(str_sub(ibge7,1,2))) %>%
              mutate(type=ifelse(year%in%c(1996,2007), "Population count",
                                 "Census"))) -> popMunic

pop2022 %>%
  group_by(uf, ibge2, year, type) %>%
  summarise(pop = sum(pop, na.rm=T)) -> popState

usethis::use_data(popMunic, overwrite = TRUE)
usethis::use_data(popState, overwrite = TRUE)

#-- atualizar info state
infoState %>%
  select(-governor, -pop)  %>%
  rename(n_munic = municip) -> infoState

usethis::use_data(infoState, overwrite = TRUE)

#-- atualizar info munic

#- litoraneos
lit <- read_excel("./raw/litoraneos.xlsx", skip=3) %>%
  select(2) %>%
  set_names("ibge7") %>%
  mutate_all(as.numeric) %>%
  drop_na() %>%
  mutate(is_coast = TRUE)

#- fronteiricos
brd <- read_excel("./raw/fronteiricos.xlsx", skip=3) %>%
  select(2) %>%
  set_names("ibge7") %>%
  mutate_all(as.numeric) %>%
  drop_na() %>%
  mutate(is_border = TRUE)


#- atualizacao
infoMunic %>%
  rename(ibge1 = regionCode,
         region = regionName,
         ibge_meso = mesoCode,
         meso = mesoName,
         ibge_micro = mesoCode,
         micro = microName) %>%
  select(-muni) %>%
  left_join(lit, by="ibge7") %>%
  left_join(brd, by="ibge7") %>%
  mutate_at(vars(is_coast, is_border), replace_na, FALSE) -> infoMunic

usethis::use_data(infoMunic, overwrite = TRUE)



















