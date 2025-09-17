library(readxl)
library(tidyverse)

pib_uf <- 
  read_excel("pib_uf.xlsx") %>%
  janitor::clean_names()

crescimento_pib_estados <- read_delim("crescimento_pib_estados.csv", 
                                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                          grouping_mark = "."), trim_ws = TRUE)%>%
  janitor::clean_names()


#Fonte: Painel shiny da CNT
pib_br_anual <- read_delim("pib_br_anual.csv", 
    delim = ";", escape_double = FALSE, col_types = cols(setores_e_subsetores = col_skip(), 
        variacao = col_skip(), valor_referencia = col_skip(), 
        valor_constante = col_skip()), trim_ws = TRUE)


#Fonte: Ipeadata
deflator_implicito_pib <- read_csv("deflator_implicito_pib.csv", 
                                   col_types = cols(...3 = col_skip()))

names(deflator_implicito_pib) <- c("ano", "var_anual")


pib_br_2023_2024<-
  pib_br_anual %>%
  filter(ano %in% c(2023,2024))


names(pib_uf)



pib_variacao_com_projecao<-
pib_uf %>%
  rename(uf= pais_uf) %>%
  inner_join(crescimento_pib_estados)



deflatores_2023_2024<-
  deflator_implicito_pib %>%
  filter(ano %in% c(2023,2024))

pib_variacao_com_projecao_com_2023_2024 <-
  pib_variacao_com_projecao %>%
  mutate(deflator_implicito_2023 = deflatores_2023_2024$var_anual[1],
         deflator_implicito_2024 = deflatores_2023_2024$var_anual[2]
         ) %>%
  mutate(projecao_2023 = pib_em_2022 *(1+x2023_estimativa_bb/100)   * (1+deflator_implicito_2023/100)   ,
         projecao_2024 = projecao_2023  *(1+x2024_estimativa_bb/100)   * (1+deflator_implicito_2024/100))  

total_projecao_2023<-
  (pib_variacao_com_projecao_com_2023_2024 %>%
  filter(uf != "Brasil") %>%
  summarise(total = sum(projecao_2023)))$total

total_projecao_2024<-
  (pib_variacao_com_projecao_com_2023_2024 %>%
     filter(uf != "Brasil") %>%
     summarise(total = sum(projecao_2024)))$total


dados_pib_estadual_excel<-
  pib_variacao_com_projecao_com_2023_2024 %>%
  mutate(pib_br_2023 = pib_br_2023_2024$valor[1],
         pib_br_2024 = pib_br_2023_2024$valor[2],
         total_projecao_2023 = total_projecao_2023,
         total_projecao_2024 = total_projecao_2024,
         projecao_2023_ajustada = ifelse(
           uf== "Brasil",
           pib_br_2023,
           pib_br_2023*(projecao_2023/total_projecao_2023)
         ),
         projecao_2024_ajustada = ifelse(
           uf== "Brasil",
           pib_br_2024,
           pib_br_2024*(projecao_2024/total_projecao_2024)
         ),
         
         )


dados_pib_estadual_excel %>%
  writexl::write_xlsx("dados_pib_estadual_excel.xlsx")
