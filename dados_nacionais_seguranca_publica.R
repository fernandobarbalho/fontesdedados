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
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(genero )) %>%
  mutate(genero = reorder(genero, soma )) %>%
  ggplot() +
  geom_col(aes(x= soma, y= genero)) +
  theme_light() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    title = "Vítimas por gênero",
    x="",
    y=""
  )


seguranca_publica %>%
  filter(total_vitimas > 0) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(evento, genero )) %>%
  ggplot() +
  geom_col(aes(x= soma, y= evento, fill = genero)) +
  theme_light() +
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Vítimas por tipo de evento e gênero",
    fill = "Gênero",
    x="",
    y=""
  )
