library(jsonlite)
library(tidyverse)




trata_xml<- function(url){ #tipo_campo (1-código, 2-valor)
  
  map_dfr(unique(url), function(a_url){
    list_codigo<- jsonlite::fromJSON(a_url)
    df<- list_codigo[[2]]
    df<- df[,1:2]
    df$url <- a_url
    names(df)[3]<- paste0("url_", names(df)[1])
    df
  })
  
}

trata_xml_unidade_pai <- function(url){ #tipo_campo (1-código, 2-valor)
  
  map_dfr(unique(url), function(a_url){
    list_codigo<- jsonlite::fromJSON(a_url)
    df<- list_codigo[[2]]
    tibble(url_codigoUnidadePai= a_url, sigla_unidade_pai = df$sigla, nome_unidade_pai = df$nome )
  
  })
  
}




orgaos<- jsonlite::fromJSON(url)

df_orgaos<- orgaos[["unidades"]]

df_orgaos_trabalho<-
  df_orgaos %>%
  rename_with( ~paste0("url_",.x), starts_with("codigo"))

codigos_natureza<-
  trata_xml(df_orgaos_trabalho$url_codigoNaturezaJuridica)

codigo_poder<-
  trata_xml(df_orgaos_trabalho$url_codigoPoder)

codigo_TipoUnidade<-
  trata_xml(df_orgaos_trabalho$url_codigoTipoUnidade)

codigo_esfera<-
  trata_xml(df_orgaos_trabalho$url_codigoEsfera)

unidade_pai<-
  trata_xml_unidade_pai(df_orgaos_trabalho$url_codigoUnidadePai)

colunas_sel<- names(df_orgaos_trabalho)[str_detect(names(df_orgaos_trabalho), "url")]


orgaos_processados<-
df_orgaos_trabalho %>%
  inner_join(codigos_natureza) %>%
  inner_join(codigo_poder) %>%
  inner_join(codigo_TipoUnidade) %>%
  inner_join(codigo_esfera) %>%
  inner_join(unidade_pai) %>%
  select(-all_of(colunas_sel))






