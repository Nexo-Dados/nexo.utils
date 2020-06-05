
# CBO ---------------------------------------------------------------------

library(rvest)




# Raspagem ----------------------------------------------------------------

rasp <- c()

for (i in seq_along(x3$CBO4)) {

  
url <- paste0("https://cbo.tellesecosta.com.br/ocupacao/", unique(x3$CBO4)[i])

raspagem <- ""

try(raspagem <- read_html(url) %>% 
  html_node(css='h1') %>% 
  html_text())

rasp <- rbind(raspagem, rasp)

print(i)
}


# Export ------------------------------------------------------------------

data <- data.frame(rasp, rev(unique(x3$CBO4)))

write.csv(data, 'cbo.csv', row.names = F)


