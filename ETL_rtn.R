library(ckanr)
library(readxl)


tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan")

tb_ckan$url


URL_add <- tb_ckan$url

tmp = tempfile(fileext = ".xlsx")

download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")

serie_header <- read_excel(tmp, sheet = "1.1-A", 
                           range = "A1:A3")



# install.packages("devtools")
devtools::install_github("tchiluanda/rtn")

library(rtn)

rtn::get_account_data_by_month(dados_rtn$Rubrica[64], month = 1:12) %>%
  rtn::plot_rtn_series()



dados_rtn<-
  rtn::get_full_data()


glimpse(dados_rtn)



dados_rtn %>%
  rename(data_apuracao = Data,
         ordem_relatorio = id,
         valor_atualizado_ipca = valor_atualizado ) %>%
  mutate(tipo = case_when(
    tipo == "R" ~ "Receita",
    tipo == "D" ~ "Despesa",
    is.na(tipo) ~"Resultado, juros ou ajuste"
  ) 
  ) %>%
  write_csv("dados_resultado_tesouro_nacional.csv")

dados_rtn %>%
  write_csv("dados_rtn.csv")
