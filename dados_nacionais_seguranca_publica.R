library(readxl)
library(tidyverse)
seguranca_publica <- BancoVDE_2024 <- read_excel("BancoVDE 2024.xlsx", 
                                                 col_types = c("text", "text", "text", 
                                                               "date", "text", "text", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "text", "text", 
                                                               "numeric"))

amostra_cidades <- c("SÃO PAULO", "RIO DE JANEIRO", "SALVADOR", "FORTALEZA", "BELO HORIZONTE", 
             "CURITIBA", "MANAUS", "BRASÍLIA", "RECIFE", "PORTO ALEGRE", 
             "GOIÂNIA", "BELÉM", "CAMPINAS", "SÃO LUÍS", "GUARULHOS", 
             "MACEIÓ", "DUQUE DE CAXIAS", "NATAL", "SÃO BERNARDO DO CAMPO", "TERESINA")

amostra_seguranca_publica<-
  seguranca_publica %>%
  filter (municipio %in% amostra_cidades) %>%
  slice_sample(n=500)

amostra_seguranca_publica %>%
  readr::write_csv("amostra_seguranca_publica.csv")

top_10_eventos<- 
  seguranca_publica %>%
  summarise(total = sum(total_vitimas),
            .by = evento) %>%
  slice_max(n=10, order_by = total)
  

seguranca_publica %>%
  filter(total_vitimas > 0) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "sexo", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(sexo )) %>%
  mutate(sexo = reorder(sexo, soma )) %>%
  ggplot() +
  geom_col(aes(x= soma, y= sexo)) +
  theme(
    legend.position = "bottom"
  )


seguranca_publica %>%
  filter(total_vitimas > 0) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "sexo", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(evento, sexo )) %>%
  ggplot() +
  geom_col(aes(x= soma, y= evento, fill = sexo)) +
  theme(
    legend.position = "bottom"
  )
