
# Libraries ---------------------------------------------------------------

library("rvest")
library("glue")
library("tidyverse")


# Estava errado -----------------------------------------------------------


ii <- 10*(0:7)
urls2 <- glue("https://www.google.com.br/search?q=site:www.nexojornal.com.br/grafico+estava+errado&ei=RR8PW_mqHMq5wAT004DwDA&start={ii}&sa=N&biw=1855&bih=941", ii=ii)

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