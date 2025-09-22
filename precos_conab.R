library(readxl)
library(tidyverse)

Consulta_precos_mensal <- 
  read_excel("Consulta-precos-mensal.xlsx", 
                                     skip = 12) %>%
  janitor::clean_names() %>%
  mutate(preco_medio = substr(preco_medio,4,20)) %>%
  mutate(preco_medio = str_replace(preco_medio,"," ,".")) %>%
  mutate(preco_medio = as.numeric(preco_medio)) %>%
  fill(produto, nivel_de_comercializacao, uf,  .direction = "down")



read_excel("Consulta-precos-mensal.xlsx", 
           skip = 12) %>%
  janitor::clean_names() %>%
  mutate(preco_medio = substr(preco_medio,4,20)) %>%
  mutate(preco_medio = str_replace(preco_medio,"," ,".")) %>%
  mutate(preco_medio = as.numeric(preco_medio)) %>%
  fill(produto, nivel_de_comercializacao, uf,  .direction = "down") %>%
  writexl::write_xlsx("consulta_preco_medio_ajustado.xlsx")




