
# Libraries ---------------------------------------------------------------

library("rvest")
library("glue")
library("tidyverse")

# Graficos ----------------------------------------------------------------

i <- 10*(0:58)
urls <- glue("https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico+grafico&ei=FQEPW9rnF8quwgS0xITABw&start={i}&sa=N&biw=1672&bih=921", i=i)

# Estava errado -----------------------------------------------------------

ii <- 10*(0:7)
urls2 <- glue("https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico+estava+errado&ei=RR8PW_mqHMq5wAT004DwDA&start={ii}&sa=N&biw=1855&bih=941", ii=ii)


# Links -------------------------------------------------------------------

websites_final <- c()

for (j in seq_along(urls)) {
main.page <- read_html(x = urls[j])

links <- main.page %>% 
  html_nodes(".r a") %>% # get the a nodes with an r class
  html_attr("href") # get the href attributes

#clean the text  
links = gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))

# as a dataframe
websites <- data.frame(links = links, stringsAsFactors = FALSE)
websites_final <- rbind(websites, websites_final)
}

# Correct the data frame
websites_final %>% 
  mutate(data = str_sub(links, 39, 48), 
         dia = str_sub(data, -2, -1),
         mes = str_sub(data, -5, -4),
         ano = str_sub(data, 1, 4),
         texto = str_sub(links, 50, str_length(links))) %>% 
  mutate(texto = str_replace_all(texto, "-", " "),
         texto = str_replace_all(texto, "%25C3%25AA", "ê"),
         texto = str_replace_all(texto, "%25C3%25A3", "ã"),
         texto = str_replace_all(texto, "%25C3%25B3", "ó"),
         texto = str_replace_all(texto, "%25C3%25A1", "á"),
         texto = str_replace_all(texto, "%25C3%25A7", "ç"),
         texto = str_replace_all(texto, "%25E2%2580%2598", "‘"),
         texto = str_replace_all(texto, "%25E2%2580%2599", "’"),
         texto = str_replace_all(texto, "%25C3%25A9", "é"),
         texto = str_replace_all(texto, "%25C3%25AD", "í"),
         texto = str_replace_all(texto, "%25C3%25A0", "à"),
         texto = str_replace_all(texto, "%25C3%25A2", "â"),
         texto = str_replace_all(texto, "%25C3%25B5", "õ"),
         texto = str_replace_all(texto, "%25C3%25B4", "ô"),
         texto = str_replace_all(texto, "%25C2%25BA", "º"),
         texto = str_replace_all(texto, "%25C3%2581", "Á"),
         texto = str_replace_all(texto, "%25C3%25BA", "ú"),
         texto = str_replace_all(texto, "%25C2%25AA", "ª"),
         length = str_length(texto)) -> websites_final2




# Estava errado -----------------------------------------------------------

estava <- c()

for (j in seq_along(urls2)) {
  main.page <- read_html(x = urls2[j])
  
  links <- main.page %>% 
    html_nodes(".r a") %>% # get the a nodes with an r class
    html_attr("href") # get the href attributes
  
  #clean the text  
  links = gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))
  
  # as a dataframe
  websites <- data.frame(links = links, stringsAsFactors = FALSE)
  estava <- rbind(websites, estava)
}

estava %>% 
  mutate(estava_errado = 1) -> estava

# Joining -----------------------------------------------------------------

graficos <- websites_final2 %>% 
  filter(links != "https://www.nexojornal.com.br/grafico/") %>% 
  left_join(estava, by="links") %>% 
  mutate(estava_errado = ifelse(is.na(estava_errado), 0, 1)) %>% 
  mutate(dia_da_semana = weekdays(lubridate::ymd(data)))


# Autor -------------------------------------------------------------------

graficos %>% 
  mutate(links2 = paste0("https://www.nexojornal.com.br/grafico/",
                                data, "/",
                                str_replace_all(texto, " ", "-"))) -> graficos2

x2 <- c()

for (m in seq_along(graficos2$links2)) {

## author
graficos2$links2[m] %>%
  read_html() %>%
  rvest::html_nodes(css = ".author") %>%
  html_text() -> x

x <- data.frame(autor=x[2], links=graficos2$links2[m])

x2 <- rbind(x, x2)
if (m %% 5 == 0) { print(m)}
}


# Final binding -----------------------------------------------------------

graficos_final <- graficos2 %>% 
  left_join(x2, by=c("links2"="links")) %>% 
  set_names("URL 1", "DATA (Y-M-D)", "DIA", "MÊS", "ANO",
            "TEXTO", "CARACTERES", "ESTAVA ERRADO", "DIA DA SEMANA",
            "URL 2", "AUTOR") %>% 
  mutate(RODOLFO = as.integer(grepl("Rodolfo Almeida", AUTOR)),
         `GABRIEL Z` = as.integer(grepl("Gabriel Zanlorenssi", AUTOR)),
         `GABRIEL M` = as.integer(grepl("Gabriel Maia", AUTOR)),
         DANIEL = as.integer(grepl("Daniel Mariani", AUTOR)),
         SIMON = as.integer(grepl("Simon", AUTOR)),
         VITORIA = as.integer(grepl("Vitória O", AUTOR)),
         CATARINA = as.integer(grepl("Catarina P", AUTOR)),
         RALPH = as.integer(grepl("Ralph M", AUTOR)),
        BEATRIZ = as.integer(grepl("Beatriz D", AUTOR)),
        JULIA = as.integer(grepl("Júlia R", AUTOR)),
        THIAGO = as.integer(grepl("Thiago Q", AUTOR)),
        FALCAO = as.integer(grepl("Guilherme F", AUTOR)),
        RENATA = as.integer(grepl("Renata R", AUTOR)),
        MARINA = as.integer(grepl("Marina M", AUTOR)),
        ARIEL = as.integer(grepl("Ariel T", AUTOR)),
        RICARDO = as.integer(grepl("Ricardo M", AUTOR)),
        LILIAN = as.integer(grepl("Lilian V", AUTOR)),
        TATIANA = as.integer(grepl("Tatiana D", AUTOR)),
        ANDRE = as.integer(grepl("André C", AUTOR)),
        EDUARDO = as.integer(grepl("Eduardo M", AUTOR)),
        IBRAHIM = as.integer(grepl("Ibrahim", AUTOR)),
        `JOSE ROBERTO` = as.integer(grepl("José Roberto C", AUTOR)),
        MURILO = as.integer(grepl("Murilo", AUTOR)),
        WELLINGTON = as.integer(grepl("Wellington F", AUTOR)),
        RAFAEL = as.integer(grepl("Rafael I", AUTOR)))

# Export ------------------------------------------------------------------


write.csv(graficos_final, "websites_final2.csv", row.names=F)



# Net ---------------------------------------------------------------------


net = rgraph(10, mode = "graph", tprob = 0.5)
net = network(net, directed = FALSE)

# vertex names
network.vertex.names(net) = letters[1:10]
  


# Graficos ----------------------------------------------------------------

library(readxl)

graficos <- read_excel("Gráficos.xlsx")


# Estava errado -----------------------------------------------------------

graficos %>% 
  group_by(`Estava Errado`, Gravidade, Mês, Ano) %>% 
  summarise(n=n()) -> x

x %>% 
  mutate(Data = lubridate::ymd(paste(Ano, Mês, "1", sep="/"))) %>% 
  tbl_df() %>% 
  mutate(Gravidade = as.numeric(Gravidade),
         Gravidade = ifelse(is.na(Gravidade), 0, Gravidade),
         `Estava Errado2` = Gravidade) %>% 
  mutate(`Estava Errado` = as.factor(`Estava Errado2`)) %>% 
  ggplot(aes(x=Data, fill=`Estava Errado`, y=n)) +
  geom_col(position="fill") +
  scale_fill_manual(values = c("#00CDCD", "#EE3B3B", "#CD3333", "#8B2323")) +
  ggthemes::theme_fivethirtyeight() +
  labs(title="Estava errado por mês")









