install.packages("tidyverse")

if (!require("remotes")) {
  install.packages("remotes")
}
#on 64-bit Windows
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
#elsewhere
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

library(tidyverse)

library(tabulizer)


mcr2030_brasil<- tabulizer::extract_tables(file = "ListaMCR2030Atualizao20231201.pdf")


df_mcr2030_brasil<-
purrr::map_dfr(1:length(mcr2030_brasil), function(i){
  table<- as.data.frame(mcr2030_brasil[[i]])
  
  names_table<- table[1,]
  
  table<- table[2:NROW(table),]
  
  names(table) <- names_table
  
  table<- janitor::clean_names(table)
  
  table$adesao <- as.Date(table$adesao,  format = "%d/%m/%Y")
  
  table
  
})


