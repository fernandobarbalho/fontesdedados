library(readxl)
library(tidyverse)
library(jsonlite)

#Dados de emissão de gases de efeito estufa e receita utilizados no Índice Carbono Eficiente (ano-base 2021)
#fonte: https://www.b3.com.br/pt_br/market-data-e-indices/indices/indices-de-sustentabilidade/indice-carbono-eficiente-ico2-emissao-de-gases.htm

ico2_b3 <- read_excel("ico2_b3.xlsx")

ico2_b3<- janitor::clean_names(ico2_b3)



empresas_setores <- read_excel("empresas_brasileiras_nova_classificacao.xlsx")
empresas_setores<- janitor::clean_names(empresas_setores)

ico2_b3_empresas<-
ico2_b3 %>%
  left_join(
    empresas_setores %>%
      rename(empresa = nome_da_empresa,
             setor = nova_classificacao_de_setor)
  )






  
empresas_open_sustentability<-  
map_dfr(0:6, function(inc){
  
  inicio<- 1+ inc*100
  fim<- (inc+1) *100
  
  
  print(inc)
  
  url<- paste0("https://api.opensustainabilityindex.org/v1/companies?api-key=demo&limit=",fim,"&offset=",inicio)
  #vide documentação da APE em https://www.opensustainabilityindex.org/api
  
  print(url)
  
  indices_open_sustentability<-
    jsonlite::fromJSON(url)
  
  indices_open_sustentability[["data"]]
  
  
})






#valor médio do dólar em 2021
valor_dolar_medio<- 5.3950 #fonte: http://www.ipeadata.gov.br/ExibeSerie.aspx?serid=31924



de_para_categorias <- read_csv("Tabela_Atualizada_de_Tradu__o_de_Categorias.csv")

de_para_categorias <- janitor::clean_names(de_para_categorias)


de_para_categorias$categoria_em_portugues

saveRDS(empresas_open_sustentability, "empresas_open_sustentability.rds")


empresas_open_sustentability %>%
  filter(is.na(industry) | is.na(hq_country)) %>%
  select(company_name, industry, hq_country) %>%
  rename(empresa = company_name,
         setor = industry,
         pais_sede = hq_country) %>%
  distinct(empresa, setor, pais_sede) %>%
  readr::write_csv("empresas_setor_pais.csv")
  
de_para_categorias %>%
  select(categoria_em_portugues)%>%
  rename(setor_economia = categoria_em_portugues) %>%
  readr::write_csv("setor_economia.csv")
