library(sidrar)
library(tidyverse)

info_sidra(9883, wb = TRUE)

favelas_municipios<-  
  get_sidra(x = 9883,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 9910,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_trabalho<-
  favelas_municipios %>%
  select(c(4:6)) %>%
  rename(
    quantidade = V,
    cod_ibge = D1C,
    municipio = D1N
  )
