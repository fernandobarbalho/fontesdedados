library(tidyverse)


url<- "https://dadosabertos.cidades.gov.br/dataset/a09e2b21-3c1d-4c2e-9da7-bbc1c33e8e2a/resource/666ef8ca-069c-45dc-aa61-0084a5aba59d/download/mcmv_financiado_fgts-202310.csv"


download.file(url, destfile = "mcmv_fgts.csv", mode = "wb")

mcmv_fgts <- read_delim("mcmv_fgts.csv", 
                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                            grouping_mark = "."), trim_ws = TRUE)

limpar_coluna <- function(dataframe, coluna) {
  dataframe[[coluna]] <- str_remove_all(dataframe[[coluna]], "[.]")
  dataframe[[coluna]] <- str_replace_all(dataframe[[coluna]], "[,]", ".")
  dataframe[[coluna]] <- str_replace_all(dataframe[[coluna]], "-", "")
  dataframe[[coluna]] <- as.numeric(dataframe[[coluna]])
  
  return(dataframe)
}

mcmv_fgts <-
mcmv_fgts %>%
  limpar_coluna("vlr_subsidio")




url<- "https://dadosabertos.cidades.gov.br/dataset/a09e2b21-3c1d-4c2e-9da7-bbc1c33e8e2a/resource/616a58fb-8c00-4eed-aab6-8713555843a2/download/mcmv_subsidiado_ogu-202310.csv"


mcmv_ogu <- download.file(url, destfile = "mcmv_ogu.csv", mode = "wb")

mcmv_ogu <- read_delim("mcmv_ogu.csv", delim = ";", 
                                   escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                  
                                                                                                                  grouping_mark = "."), trim_ws = TRUE)
mcmv_ogu<-
mcmv_ogu %>%
  limpar_coluna("qtd_uh_vigentes")

###Endereço para consulta de propostas aceitas dentro do MCMV-FAR
https://www.in.gov.br/en/web/dou/-/portaria-mcid-n-1.482-de-21-de-novembro-de-2023-524905456

