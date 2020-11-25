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

usethis::use_data(infoMunic, overwrite = TRUE)



