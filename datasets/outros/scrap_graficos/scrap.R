
# Libraries ---------------------------------------------------------------

library("rvest")
library("glue")
library("tidyverse")


# Google URLs -------------------------------------------------------------

## results -- CORRIGIR
n_res = 450
n_res = n_res / 10

## number of pages -- CORRIGIR
lista <- list(url_2019 = "https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&tbs=cdr:1,cd_min:1/1/2019,cd_max:12/31/2019&ei=PIT2XNO_Iome5gLB2oT4Cw&start={i}&sa=N&ved=0ahUKEwjTzZTaiNDiAhUJj1kKHUEtAb8Q8tMDCG8&biw=1352&bih=756",
              url_2018 = "https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&tbs=cdr:1,cd_min:1/1/2018,cd_max:12/31/2018&ei=9oL2XM3nA87b5gLhsaeQDA&start={i}&sa=N&ved=0ahUKEwjNury-h9DiAhXOrVkKHeHYCcIQ8tMDCG4&biw=1352&bih=756",
              url_2017 = "https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&tbs=cdr:1,cd_min:1/1/2017,cd_max:12/31/2017&ei=W4P2XM7oOIOd5gLj0oDIDg&start={i}&sa=N&ved=0ahUKEwiOgobvh9DiAhWDjlkKHWMpAOkQ8tMDCG8&biw=1352&bih=756",
              url_2016 = "https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&tbs=cdr:1,cd_min:1/1/2016,cd_max:12/31/2016&ei=jIP2XJf4Ie7n5gKDop6wAQ&start={i}&sa=N&ved=0ahUKEwiX7p2GiNDiAhXus1kKHQORBxYQ8tMDCG8&biw=1352&bih=756",
              url_2015 = "https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&tbs=cdr:1,cd_min:1/1/2015,cd_max:12/31/2015&ei=1IP2XJ6LCYGW5wL866qIAw&start={i}&sa=N&ved=0ahUKEwiexa-oiNDiAhUBy1kKHfy1CjEQ8tMDCHA&biw=1352&bih=756")

i <- 10*(0:n_res)

urls <- map(lista, function(x) glue(x, i=i)) %>% 
  flatten_chr()
  
urls <- glue("https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico&ei=CoL2XJ2RGa345gL7wYbwBA&start={i}&sa=N&ved=0ahUKEwidvo3OhtDiAhUtvFkKHfugAU44ChDy0wMIcA&biw=1097&bih=756", i=i)

## 


# Links -------------------------------------------------------------------

## Function to scrap links
fx <- function(x) {
  read_html(x) %>% 
    html_nodes("a") %>% # get the a nodes with an r class
    html_attr("href") -> links # get the href attributes
  websites <- data.frame(links = gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1)),
                         stringsAsFactors = FALSE)
  Sys.sleep(runif(1)*9)
  return(websites)
}

## Map
textos <- map(urls, fx)

## Just for safety 
textos2 <- textos %>% 
  bind_rows() %>% 
  as.data.frame() %>% 
  filter(str_detect(links, "nexojornal.com.br")) %>% 
  filter(!str_detect(links, "search")) %>% 
  filter(!str_detect(links, "account"))

# Nexo --------------------------------------------------------------------


# Correct the data frame
textos2 %>% 
  mutate(date = str_sub(links, 39, 48), 
         day = str_sub(date, -2, -1),
         month = str_sub(date, -5, -4),
         year = str_sub(date, 1, 4),
         text = str_sub(links, 50, str_length(links))) %>% 
  mutate(text = str_replace_all(text, "-", " "),
         text = str_replace_all(text, "%25C3%25AA", "ê"),
         text = str_replace_all(text, "%25C3%25A3", "ã"),
         text = str_replace_all(text, "%25C3%25B3", "ó"),
         text = str_replace_all(text, "%25C3%25A1", "á"),
         text = str_replace_all(text, "%25C3%25A7", "ç"),
         text = str_replace_all(text, "%25E2%2580%2598", "‘"),
         text = str_replace_all(text, "%25E2%2580%2599", "’"),
         text = str_replace_all(text, "%25C3%25A9", "é"),
         text = str_replace_all(text, "%25C3%25AD", "í"),
         text = str_replace_all(text, "%25C3%25A0", "à"),
         text = str_replace_all(text, "%25C3%25A2", "â"),
         text = str_replace_all(text, "%25C3%25B5", "õ"),
         text = str_replace_all(text, "%25C3%25B4", "ô"),
         text = str_replace_all(text, "%25C2%25BA", "º"),
         text = str_replace_all(text, "%25C3%2581", "Á"),
         text = str_replace_all(text, "%25C3%25BA", "ú"),
         text = str_replace_all(text, "%25C2%25AA", "ª"),
         length = str_length(text)) %>% 
 mutate(weekday = weekdays(lubridate::ymd(date))) %>%
 mutate(links2 = paste0("https://www.nexojornal.com.br/grafico/",
                         date, "/",
                         str_replace_all(text, " ", "-"))) -> graficos

# Author ------------------------------------------------------------------

# function to scrap nexo's website
fy <- function(x) {
  x %>% 
    read_html() %>% 
    html_nodes(css = ".author") %>% 
    html_text() -> text
  return(text)
}

# safe version
safe_fy <- safely(fy)

authors <- map(graficos$links2[1:2], safe_fy)



x <- data.frame(autor=x[2], links=graficos2$links2[m])

x2 <- rbind(x, x2)

# # Final binding -----------------------------------------------------------
# 
# graficos_final <- graficos2 %>% 
#   left_join(x2, by=c("links2"="links")) %>% 
#   set_names("URL 1", "DATA (Y-M-D)", "DIA", "MÊS", "ANO",
#             "TEXTO", "CARACTERES", "ESTAVA ERRADO", "DIA DA SEMANA",
#             "URL 2", "AUTOR") %>% 
#   mutate(RODOLFO = as.integer(grepl("Rodolfo Almeida", AUTOR)),
#          `GABRIEL Z` = as.integer(grepl("Gabriel Zanlorenssi", AUTOR)),
#          `GABRIEL M` = as.integer(grepl("Gabriel Maia", AUTOR)),
#          DANIEL = as.integer(grepl("Daniel Mariani", AUTOR)),
#          SIMON = as.integer(grepl("Simon", AUTOR)),
#          VITORIA = as.integer(grepl("Vitória O", AUTOR)),
#          CATARINA = as.integer(grepl("Catarina P", AUTOR)),
#          RALPH = as.integer(grepl("Ralph M", AUTOR)),
#         BEATRIZ = as.integer(grepl("Beatriz D", AUTOR)),
#         JULIA = as.integer(grepl("Júlia R", AUTOR)),
#         THIAGO = as.integer(grepl("Thiago Q", AUTOR)),
#         FALCAO = as.integer(grepl("Guilherme F", AUTOR)),
#         RENATA = as.integer(grepl("Renata R", AUTOR)),
#         MARINA = as.integer(grepl("Marina M", AUTOR)),
#         ARIEL = as.integer(grepl("Ariel T", AUTOR)),
#         RICARDO = as.integer(grepl("Ricardo M", AUTOR)),
#         LILIAN = as.integer(grepl("Lilian V", AUTOR)),
#         TATIANA = as.integer(grepl("Tatiana D", AUTOR)),
#         ANDRE = as.integer(grepl("André C", AUTOR)),
#         EDUARDO = as.integer(grepl("Eduardo M", AUTOR)),
#         IBRAHIM = as.integer(grepl("Ibrahim", AUTOR)),
#         `JOSE ROBERTO` = as.integer(grepl("José Roberto C", AUTOR)),
#         MURILO = as.integer(grepl("Murilo", AUTOR)),
#         WELLINGTON = as.integer(grepl("Wellington F", AUTOR)),
#         RAFAEL = as.integer(grepl("Rafael I", AUTOR)))
# 
# # Export ------------------------------------------------------------------
# 
# 
# write.csv(graficos_final, "websites_final2.csv", row.names=F)















