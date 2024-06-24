library(sidrar)
library(tidyverse)

info_sidra(8888, wb = TRUE)

periodo_consulta<- c("202301-202404")


producao_industria_transformacao<-
  get_sidra(x = 8888,
            variable = c(11601),
            period = periodo_consulta,
            #geo =  "City",
            #geo.filter = "RS",
            classific = "C544",
            category =  list(c(129316)), 
            header = FALSE,
            format = 3)

producao_industria_transformacao_serie<-
  producao_industria_transformacao %>%
  mutate(ordem = row_number()) %>%
  select(ordem,D3N, D2N, V) 

names(producao_industria_transformacao_serie)[2:4] <-
  c("variável", "mes", "variacao_percentual")


producao_industria_transformacao_serie<-
producao_industria_transformacao_serie %>%
  arrange(ordem) %>%
  mutate(variacao_decimal = variacao_percentual/100,
         valor_multiplicador = 1+variacao_decimal)

valor_ant_acumulado<<- producao_industria_transformacao_serie$valor_multiplicador[1]


producao_industria_transformacao_serie<-
purrr::map_dfr(1:NROW(producao_industria_transformacao_serie), function(i){
  
  print(paste("primeiro valor acumulado", valor_ant_acumulado ))
  
  valor_multiplicador_acumulado<- ifelse(i==1, valor_ant_acumulado, valor_ant_acumulado * producao_industria_transformacao_serie$valor_multiplicador[i] )
  
  valor_ant_acumulado<<- valor_multiplicador_acumulado
  print(paste("segundo valor acumulado", valor_ant_acumulado ))
  
  bind_cols(producao_industria_transformacao_serie[i,], tibble(valor_multiplicador_acumulado))

})


producao_industria_transformacao_serie<-
producao_industria_transformacao_serie %>%
  mutate(variacao_acumulada = (valor_multiplicador_acumulado -1 )*100,
         numero_indice = 100* valor_multiplicador_acumulado)


producao_industria_transformacao_serie %>%
  writexl::write_xlsx("producao_industria_transformacao_serie.xlsx")

101.89241/100.27999
