library(tidyverse)
setwd("~/Documentos/Graficos/Scrap Graficos/prints")
dados <- read_csv("~/Documentos/Graficos/Scrap Graficos/Gráficos - Planilha (1).csv") %>% 
  group_by(`Data (Y-M-D)`) %>% 
  mutate(rank=rank(Dia, ties.method = "first")) %>% ungroup() %>% 
  mutate(id=paste0(Ano, formatC(Mês,flag=0,width=2), formatC(Dia,flag=0,width=2),  rank)%>% str_replace_all("/", "")) %>% 
  select(-rank)

list.files()[str_detect(list.files(),".png")] %>% 
  str_replace_all(".png", "")    ->done

dados2<-filter(dados , !(id%in%done) )
for (i in 1:nrow(dados2)){
  
  webshot::webshot(
    dados2$`Url 2`[i],delay = 2,vwidth =1140,
   paste0(dados2$`id`[i], ".png"))
  Sys.sleep(3)}



