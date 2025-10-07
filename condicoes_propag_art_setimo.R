library(tidyverse)
library(readxl)

receitas_primarias_estados_2024 <- 
  read_delim("receitas_primarias_estados.csv", 
                                         delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                             grouping_mark = ".", encoding = "LATIN1"), 
                                         trim_ws = TRUE, skip = 5) %>%
  janitor::clean_names()%>%
  mutate(ano = 2024)



resultados_primarios_estados_2024 <- 
  read_delim("resultados_primarios_estados.csv", 
                                           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                               grouping_mark = ".", encoding = "LATIN1"), 
                                           trim_ws = TRUE, skip = 5) %>%
  janitor::clean_names()%>%
  mutate(ano = 2024)



receitas_primarias_estados_2023 <- 
  
read_delim("receitas_primarias_2023.csv", 
           delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                               grouping_mark = ".", encoding = "LATIN1"), 
           trim_ws = TRUE, skip = 5) %>%
  janitor::clean_names() %>%
  mutate(ano = 2023)



receitas_primarias<-
  receitas_primarias_estados_2023 %>%
  bind_rows(receitas_primarias_estados_2024)


ipca_2024<- 4.83 #valores em %


receitas_primarias_totais<-
  receitas_primarias %>%
  filter(identificador_da_conta == "siconfi-cor_RREO6TotalReceitaPrimaria",
         coluna == "RECEITAS REALIZADAS (a)") %>%
  select(uf, ano, valor) %>%
  pivot_wider(names_from = ano,
              values_from = valor,
              names_prefix = "total_receita_") %>%
  mutate(total_receita_2023_corrigida = `total_receita_2023` *(1+ipca_2024/100),
         variacao_2024_2023_corrigida = ((total_receita_2024/total_receita_2023_corrigida)-1)) %>%
  pivot_longer(cols = -uf,
               names_to = "tipo_valor",
               values_to = "valor")


resultado_primario_base<-
  resultados_primarios_estados_2024 %>%
  filter(identificador_da_conta == "siconfi-cor_ResultadoPrimarioComRPPSAcimaDaLinha") %>%
  select(uf,  valor) %>%
  mutate(tipo_valor = "resultado_primario") 

diagnostico_art_setimo_propag<-
receitas_primarias_totais %>%
  bind_rows(resultado_primario_base) %>%
  pivot_wider(names_from = tipo_valor,
              values_from = valor) %>%
  mutate(condicao_I = total_receita_2024 < total_receita_2023_corrigida,
         condicao_II = resultado_primario <=0 & !condicao_I,
         condicao_III = resultado_primario >0 & !condicao_I) %>%
  mutate(margem_despesa = case_when(
    condicao_I ~ (ipca_2024/100),
    condicao_II ~ (ipca_2024/100 + 0.5*variacao_2024_2023_corrigida),
    condicao_III ~ (ipca_2024/100 + 0.7*variacao_2024_2023_corrigida)
  ))

diagnostico_art_setimo_propag %>%
  writexl::write_xlsx("diagnostico_art_setimo_propag_new.xlsx")
  

