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

extrato_siop_trabalho %>%
  summarise(depesa_total = sum(pago),
            .by = c(ano, poder))


