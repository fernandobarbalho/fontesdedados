library(tidyverse)
library(deflateBR)

extrato_siop_filtros <- read_delim("extrato_siop_filtros.csv", 
                                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                       grouping_mark = "."), trim_ws = TRUE)

extrato_siop_filtros <- janitor::clean_names(extrato_siop_filtros)

extrato_siop_filtros <- extrato_siop_filtros[-1,]

extrato_siop_filtros %>%
  distinct(orgao_orcamentario) %>%
  readr::write_csv("orgao_orcamentario.csv")


orgao_orcamentario <- read_csv("orgao_orcamentario.csv")

extrato_siop_trabalho<-
extrato_siop_filtros %>%
  inner_join(orgao_orcamentario)


summary(extrato_siop_trabalho)

dados_grafico<-
extrato_siop_trabalho %>%
  summarise(depesa_total = sum(pago),
            .by = c(ano, poder)) %>%
  mutate(data_nominal = as.Date(paste(ano,"12","01", sep = "-")),
         despesa_total_corrigida = ipca(depesa_total,data_nominal, "11/2024")) %>%
  select(ano, poder, despesa_total_corrigida)

dados_referencia_2010<-
  dados_grafico %>%
  filter(ano==2010) %>%
  rename(depesa_total_corrigida_2010 = despesa_total_corrigida) %>%
  select(poder, depesa_total_corrigida_2010 )

dados_grafico<-
dados_grafico %>%
  inner_join(dados_referencia_2010) %>%
  mutate(evolucao =  (despesa_total_corrigida/depesa_total_corrigida_2010)*100)

ipca(100, as.Date("2023-12-01"), "11/2024")
