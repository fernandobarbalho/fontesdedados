library(readxl)
library(tidyverse)

#Dados de emissão de gases de efeito estufa e receita utilizados no Índice Carbono Eficiente (ano-base 2021)
#fonte: https://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-de-sustentabilidade/indice-carbono-eficiente-ico2-emissao-de-gases.htm

ico2_b3 <- read_excel("ico2_b3.xlsx")

ico2_b3<- janitor::clean_names(ico2_b3)

empresas_setores <- read_csv("empresas_setores.csv")
empresas_setores<- janitor::clean_names(empresas_setores)


ico2_b3_empresas<-
ico2_b3 %>%
  left_join(
    empresas_setores %>%
      rename(empresa = nome_da_empresa)
  )
