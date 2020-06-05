
setwd("~/Documentos/Graficos/Scrap Graficos/dez_2019")




df<-matrix(nrow=0,ncol = 4) %>% as.data.frame() %>% 
  set_names("names","autores","links","date")

for (i in 1:5){

rvest::html(paste0("https://www.nexojornal.com.br/grafico/?page=",i))->html

html %>% 
  rvest::html_nodes("h4 a") %>% 
  rvest::html_attr("href")->links
  

html %>% 
  rvest::html_nodes("h4 a") %>% 
  rvest::html_text()->names


html %>% 
  rvest::html_nodes('.Section__container___2ec_u') %>% 
  rvest::html_nodes("div span") %>% 
  rvest::html_text()->autores

autores<-autores[1:20]

data_frame(names, autores, links) %>% 
  mutate(links=paste0("https://www.nexojornal.com.br",links),
         date=str_remove(links,"https://www.nexojornal.com.br/grafico/"),
         date=str_sub(date,1,10) %>% lubridate::ymd())  ->bump

df<-rbind(df, bump)}


write_csv(x = df,paste0("Gráficos ",Sys.Date(),".csv"))

