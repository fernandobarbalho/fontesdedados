library(readxl)
library(tidyverse)
library(jsonlite)

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




empresas_open_sustentability<-
map_dfr(1:6, function(inc){
  
  print(inc)
  
  url<- paste("https://api.opensustainabilityindex.org/v1/companies?api-key=demo&limit=",inc,"&offset=",inc*100)
  
  print(url)
  
  indices_open_sustentability<-
    jsonlite::fromJSON("https://api.opensustainabilityindex.org/v1/companies?api-key=demo&limit=99&offset=99")
  
  indices_open_sustentability[["data"]]
  
  
})


#valor médio do dólar em 2021
valor_dolar_medio<- 5.3950 #fonte: http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=31924


unique(empresas_open_sustentability$industry)
