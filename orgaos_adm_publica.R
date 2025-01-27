library(jsonlite)


url<- "https://estruturaorganizacional.dados.gov.br/doc/orgao-entidade/resumida?codigoPoder=1&codigoEsfera=1"

orgaos<- jsonlite::fromJSON(url)

df_orgaos<- orgaos[["unidades"]]

df_orgaos$codigoNaturezaJuridica


https://estruturaorganizacional.dados.gov.br/id/natureza-juridica/4



df_orgaos %>%
  select(nome, sigla, codigoNaturezaJuridica) %>%
  mutate(codigoNaturezaJuridica = substr(codigoNaturezaJuridica, str_length(codigoNaturezaJuridica), str_length(codigoNaturezaJuridica))) %>%
  distinct(codigoNaturezaJuridica)


df_orgaos %>%
  select(nome, sigla, codigoNaturezaJuridica) %>%
  mutate(codigoNaturezaJuridica = substr(codigoNaturezaJuridica, str_length(codigoNaturezaJuridica), str_length(codigoNaturezaJuridica))) %>%
  mutate(natureza_juridica = case_when(
    codigoNaturezaJuridica == "1" ~"Empresa Pública",
    codigoNaturezaJuridica == "2" ~"Fundação Pública", 
    codigoNaturezaJuridica == "3" ~"Administração Direta",
    codigoNaturezaJuridica == "4" ~"Autarquia",
    codigoNaturezaJuridica == "6" ~"Sociedade de Economia Mista",
    codigoNaturezaJuridica == "7" ~"Órgão Público Autônomo Federal"
    
  )) %>%
  writexl::write_xlsx("orgaos_adm_publica.xlsx")

  

substr("https://estruturaorganizacional.dados.gov.br/id/natureza-juridica/4", str_length("https://estruturaorganizacional.dados.gov.br/id/natureza-juridica/4"), str_length("https://estruturaorganizacional.dados.gov.br/id/natureza-juridica/4"))
