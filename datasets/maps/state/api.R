
# Library -----------------------------------------------------------------

require(httr)
require(glue)
require(lubridate)


# Unidades ----------------------------------------------------------------

unidade = 110574


# Dias do ano -------------------------------------------------------------

x <- dmy('01-01-2016') + 0:365

days <- paste0(str_pad(day(x), 2, pad="0"), "%2F", str_pad(month(x), 2, pad="0"), "%2F", year(x))
pagina  = 1
fase = 1

Y <- list()

for (i in seq_along(days)) {

X <- GET(glue("http://www.transparencia.gov.br/api-de-dados/despesas/documentos?unidadeGestora={unidade}&dataEmissao={data1}&fase={fase}&pagina={pagina}",
              data1=days[i], pagina=pagina, fase=fase, unidade=unidade)) %>% 
  content()

Y[[i]] <- map_df(X, as.data.frame)
print(i)
}

Y %>% 
  bind_rows() -> Z
