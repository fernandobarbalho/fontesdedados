library(tidyverse)
library(readxl)



trata_tabela<- function(ds_name, index_name){
  ds <- read_excel(ds_name)
  
  ds<- ds[-1,]
  
  names(ds) <- c("pais","indice")
  
  ds$tipo_indice<- index_name
  
  ds<- filter(ds,!is.na(indice))
  
  ds
  
}
  



#trata índice de preparo para IA
indice_preparo_ia<- trata_tabela("imf-dm-export-20250102.xls", "preparo_ia" )


#trata índice de infraestrutura digital
infra_estrutura_digital<- trata_tabela("imf-dm-export-20250102(1).xls", "infraestrutura_digital")


#Importa índice de inovação e integração econômica
inovacao_integracao_ecomomica<- trata_tabela("imf-dm-export-20250102(2).xls", "inovacao_integracao_ecomomica")


#Importa índice de regulação e ética
regulacao_etica<- trata_tabela("imf-dm-export-20250102(2).xls", "regulacao_etica")

consolidado_preparo_ia<-
  bind_rows(
    indice_preparo_ia,
    infra_estrutura_digital,
    inovacao_integracao_ecomomica,
    regulacao_etica
  )


