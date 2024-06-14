library(tidyverse)
library(readxl)


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


download.file(url, destfile = "mcmv_ogu.csv", mode = "wb")

mcmv_ogu <- read_delim("mcmv_ogu.csv", delim = ";", 
                                   escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                  
                                                                                                                  grouping_mark = "."), trim_ws = TRUE)
mcmv_ogu<-
mcmv_ogu %>%
  limpar_coluna("qtd_uh_vigentes")


saveRDS(mcmv_fgts, "mcmv_fgts.rds" )

saveRDS(mcmv_ogu, "mcmv_ogu.rds")

###Endereço para consulta de propostas aceitas dentro do MCMV-FAR
https://www.in.gov.br/en/web/dou/-/portaria-mcid-n-1.482-de-21-de-novembro-de-2023-524905456

#Endereço com valor de casas e apartamentos do MCMV-FAR
https://www.gov.br/cidades/pt-br/acesso-a-informacao/acoes-e-programas/habitacao/arquivos-1/VALORESUHMCMVFARFDS_novo_CENSO_2023_vf.xlsx



##Propostas para o programa pró-moradia

url<- "http://dadosabertos.cidades.gov.br/dataset/40298ef4-7fe0-4921-aed8-3ff369087de0/resource/9751a5d2-63a7-47a2-9038-d6b3a43d7286/download/base-selehab-julho-2022.csv"

download.file(url, destfile = "SELEHAB.csv", mode = "wb")

url_ckan<- "https://dadosabertos.cidades.gov.br/"

id_recurso<- "9751a5d2-63a7-47a2-9038-d6b3a43d7286"


tb_ckan<-
ckanr::resource_show(id= id_recurso, url = url_ckan)

tb_ckan$description

url_resource<- "https://dadosabertos.cidades.gov.br/dataset/40298ef4-7fe0-4921-aed8-3ff369087de0/resource/9751a5d2-63a7-47a2-9038-d6b3a43d7286/download/base-selehab-julho-2022-csv.csv" 

download.file(url_resource, destfile = "SELEHAB.csv", mode = "wb")


selehab_ate_15jul2022 <- read_csv("SELEHAB.csv")

selehab_ate_15jul2022 <- janitor::clean_names(selehab_ate_15jul2022)

saveRDS(selehab_ate_15jul2022, "selehab_ate_15jul2022.rds")



alerta_mapbiomas <- read_excel("RAD2023_ALL_Alerts_2019-2023 - SITE.xlsx", 
                                                sheet = "BD_RAD2022_ALERTAS_2019-2023")

saveRDS(alerta_mapbiomas, "alerta_mapbiomas.RDS")

unique(alerta_mapbiomas$vector_pressure)

alerta_mapbiomas %>%
  filter(vector_pressure == "urban_expansion") %>%
  summarise(total_estado = sum(state_area), 
            quantidade_ocorrencias = n(),
            area_ocorrencia = sum(state_area)/n(),
            .by = state_name) %>%
  arrange(desc(area_ocorrencia))


alerta_mapbiomas %>%
  filter(vector_pressure == "urban_expansion") %>%
  summarise(quantidade_ocorrencias = n(),
            total_estado = sum(state_area), 
            area_ocorrencia = sum(state_area)/n(),
            .by = state_name) %>%
  arrange(desc(quantidade_ocorrencias))


alerta_mapbiomas %>%
  filter(vector_pressure == "urban_expansion") %>%
  summarise(total_estado = sum(state_area),
            quantidade_ocorrencias = n(),
            area_ocorrencia = sum(state_area)/n(),
            .by = state_name) %>%
  arrange(desc(total_estado))


alerta_mapbiomas %>%
  filter(vector_pressure == "urban_expansion",
         city_name == "Vilhena",
         state_name == "RO") %>%
  summarise(area_total_cidade = sum(city_area),
            .by = year_detected) %>%
  ggplot() +
  geom_col(aes(x= year_detected, y= area_total_cidade)) +
  labs(
    title = "Desmatamento por pressão de expansão urbana",
    subtitle = "Vilhena - RO"
  )


alerta_mapbiomas %>%
  filter(city_name == "Vilhena",
         state_name == "RO",
         year_detected == 2023) %>%
  summarise(area_total_cidade = sum(city_area),
            quantidade_ocorrencias = n(),
            area_ocorrencia = mean(city_area),
            .by = vector_pressure) %>%
  arrange(desc(area_total_cidade))




alerta_mapbiomas %>%
  filter(vector_pressure == "urban_expansion") %>%
  summarise(total_estado = sum(state_area))
            

sub_set_modelo<-
alerta_mapbiomas %>%
  filter(vector_pressure %in% c("urban_expansion", "agriculture")) %>%
  summarise(area_total_cidade = sum(city_area),
            .by = c(vector_pressure, city_name, state_name, year_detected, biome_name)) %>%
  pivot_wider(names_from = vector_pressure, values_from = area_total_cidade) %>%
  mutate(year_detected =as.factor(year_detected)) %>%
  filter(urban_expansion > 0) %>%
  select(-city_name) %>%
  mutate(agriculture = log(agriculture) )


cor.test(log(sub_set_modelo$urban_expansion), log(sub_set_modelo$agriculture))
modelo<- lm(urban_expansion~., data = sub_set_modelo)

summary(modelo)

library(performance)



performance::check_model(modelo)








modelo2<- lm(log(urban_expansion)~., data = sub_set_modelo)

summary(modelo2)


performance::check_model(modelo2)

alerta_mapbiomas %>%
  filter(vector_pressure %in% c("urban_expansion", "agriculture")) %>%
  summarise(area_total_cidade = sum(city_area),
            .by = c(vector_pressure, city_name, state_name, year_detected, biome_name)) %>%
  pivot_wider(names_from = vector_pressure, values_from = area_total_cidade) %>%
  mutate(year_detected =as.factor(year_detected)) %>%
  select(-city_name) %>%
  ggplot() +
  geom_point(aes(x=agriculture, y=urban_expansion))+
  scale_x_log10() +
  scale_y_log10()

