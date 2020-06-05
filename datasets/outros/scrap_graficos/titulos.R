
# Libraries ---------------------------------------------------------------

library("rvest")
library("glue")
library("tidyverse")

# Graficos ----------------------------------------------------------------

presid <- c("+Fernando+Haddad", "+Ciro+Gomes", "+Jair+Bolsonaro",
            "+Guilherme+Boulos", "+")

i <- 10*(0:33)
urls <- glue("https://www.google.com.br/search?q=site:https://www.nexojornal.com.br/expresso/+Fernando+Haddad&ei=sbq3W-O8HIWAwgTX8rmwDg&start={i}&sa=N&biw=1323&bih=871", i=i)


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
  mutate(data = str_sub(links, 40, 49), 
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


# Joining -----------------------------------------------------------------

graficos <- websites_final2 %>% 
  mutate(dia_da_semana = weekdays(lubridate::ymd(data)))


graficos %>%
  group_by(mes, ano) %>% 
  summarise(n=n()) %>% 
  mutate(data = lubridate::dmy(paste('1',mes,ano,sep="-"))) %>% 
  ggplot(aes(x=data, y=n)) +
  geom_col()
  





