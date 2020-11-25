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
