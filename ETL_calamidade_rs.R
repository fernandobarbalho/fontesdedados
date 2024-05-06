# Carregar os pacotes necessários
library(rvest)
library(tidyverse)

# Especificar a URL da página de onde você quer extrair a tabela
url <- "https://www.in.gov.br/en/web/dou/-/portaria-n-1.377-de-5-de-maio-de-2024-557715939"

# Ler o conteúdo da página
pagina <- read_html(url)

# Extrair a tabela
# Suponha que queremos a primeira tabela, então usamos html_table()[[1]]
# Se houver mais tabelas, você pode ajustar o índice para a tabela desejada
tabela <- pagina %>%
  html_table(fill = TRUE) %>%
  .[[1]]  # Altere o índice aqui se necessário

names(tabela) <- c("seq_mun", "municipios")

tabela$uf<- "RS"

saveRDS(tabela, "municpios_calamidade.rds")

tabela %>%
  writexl::write_xlsx("municipios_calamidade.xlsx")


municipios_calamidade_ibge <- read_csv("municipios_calamidade_ibge.csv")

municipios_calamidade_ibge%>%
  writexl::write_xlsx("municipios_calamidade.xlsx")


saveRDS(municipios_calamidade_ibge, "municipios_calamidade_ibge.rds")