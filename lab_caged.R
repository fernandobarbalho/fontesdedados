library(tidyverse)
library(colorspace)
seeg_liquido <- read_csv("SEEG_liquido.csv")


seeg_liquido_trabalho <-
  seeg_liquido %>%
  pivot_longer(cols = -1,
               names_to = "ano",
               values_to = "valor") 

seeg_liquido_trabalho<- janitor::clean_names(seeg_liquido_trabalho)


seeg_liquido_trabalho %>%
  ggplot(aes(x= ano, y= valor)) +
  geom_line(aes(group = categoria, color= categoria))


url<- "ftp://ftp.mtps.gov.br/pdet/microdados/"

download.file(url, destfile = "CAGEDEST_raiz.html")

url<- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/Leia-me.txt"

download.file(url, destfile = "Leia-me.txt")


url<- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/2024/202409/CAGEDFOR202409.7z"

download.file(url, destfile = "CAGEDFOR202409.7z")




CAGEDMOVAAAAMM

/pdet/microdados/NOVO%20CAGED/

url<- "ftp://ftp.mtps.gov.brpdet/pdet/microdados/NOVO CAGED/"
download.file(url, destfile = "novo_caged_raiz.html")

    

url<- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/2019/CAGEDEST_122019.7z"
  
download.file(url, destfile = "CAGEDEST_122019.7z")


url<- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/2019/CAGEDEST_092024.7z"

download.file(url, destfile = "CAGEDEST_122019.7z")
