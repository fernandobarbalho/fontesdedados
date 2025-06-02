library(jsonlite)
library(tidyverse)

{
  "ano": "2023",
  "periodo": "12",
  "dsPeriodo": "1º Bimestre"
},
{
  "ano": "2023",
  "periodo": "14",
  "dsPeriodo": "2º Bimestre"
},
{
  "ano": "2023",
  "periodo": "1",
  "dsPeriodo": "3º Bimestre"
},
{
  "ano": "2023",
  "periodo": "18",
  "dsPeriodo": "4º Bimestre"
},
{
  "ano": "2023",
  "periodo": "20",
  "dsPeriodo": "5º Bimestre"
},
{
  "ano": "2023",
  "periodo": "2",
  "dsPeriodo": "6º Bimestre"
}


# Vetor com os códigos IBGE das Unidades da Federação (UFs)
codigos_uf <- c(
  12, # Acre
  27, # Alagoas
  13, # Amazonas
  16, # Amapá
  29, # Bahia
  23, # Ceará
#  53, # Distrito Federal
  32, # Espírito Santo
  52, # Goiás
  21, # Maranhão
  31, # Minas Gerais
  50, # Mato Grosso do Sul
  51, # Mato Grosso
  15, # Pará
  25, # Paraíba
  26, # Pernambuco
  22, # Piauí
  41, # Paraná
  33, # Rio de Janeiro
  24, # Rio Grande do Norte
  43, # Rio Grande do Sul
  11, # Rondônia
  14, # Roraima
  42, # Santa Catarina
  28, # Sergipe
  35  # São Paulo
)


municipios_seis_digitos <- read_csv("municipios_seis_digitos.csv")



url_df<- "https://siops-consulta-publica-api.saude.gov.br/v1/rreo/df/2023/1-2"


url_uf<- "https://siops-consulta-publica-api.saude.gov.br/v1/rreo/estadual/23/2023/2"

url_base_uf<- "https://siops-consulta-publica-api.saude.gov.br/v1/rreo/estadual/23/2023/2"

siops_uf<- jsonlite::read_json(url_uf, simplifyVector = TRUE)

siops_uf_2023<-
  map_dfr(codigos_uf, function(uf){
    print(uf)
    jsonlite::read_json(paste0("https://siops-consulta-publica-api.saude.gov.br/v1/rreo/estadual/",uf,"/2023/2"))
    
  })


siops_uf_2024<-
  map_dfr(codigos_uf, function(uf){
    print(uf)
    jsonlite::read_json(paste0("https://siops-consulta-publica-api.saude.gov.br/v1/rreo/estadual/",uf,"/2023/2"))
    
  })


sipos_mun_2023<-
  
  map_dfr(municipios_seis_digitos$id_municipio_6, function(municipio){
    print(municipio)
    
    uf<- str_sub(municipio,1,2)
    
    res<- try(jsonlite::read_json(paste0("https://siops-consulta-publica-api.saude.gov.br/v1/rreo/municipal/",uf,"/",municipio,"/2023/2")))
    
    if (inherits(res,"try-error")) {
      return()
    }
    res
    
  })

#procurar por ASPS

sipos_mun_2024<-
  
  map_dfr(municipios_seis_digitos$id_municipio_6, function(municipio){
    print(municipio)
    
    uf<- str_sub(municipio,1,2)
    
    res<- try(jsonlite::read_json(paste0("https://siops-consulta-publica-api.saude.gov.br/v1/rreo/municipal/",uf,"/",municipio,"/2024/2")))
    
    if (inherits(res,"try-error")) {
      return()
    }
    res
    
  })
