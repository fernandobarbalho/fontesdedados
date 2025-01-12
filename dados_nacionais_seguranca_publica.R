library(readxl)
library(tidyverse)
library(lubridate)
library(colorspace)

https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2024-6.xlsx/@@download/file

https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2023-3.xlsx/@@download/file

https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-2022-3.xlsx/@@download/file


#2024 6
#2021 a 2023 3
#2015 a 202o 2


seguranca_publica_2024 <-
  read_excel("BancoVDE 2024.xlsx",
                                                 col_types = c("text", "text", "text",
                                                               "date", "text", "text", "numeric",
                                                               "numeric", "numeric", "numeric",
                                                               "numeric", "numeric", "text", "text",
                                                               "numeric"))
seguranca_publica_2023 <-
  read_excel("BancoVDE 2023.xlsx", 
              col_types = c("text", "text", "text", 
                            "date", "text", "text", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "numeric", "numeric", "text", "text", 
                            "numeric"))
seguranca_publica_2022 <-
  read_excel("BancoVDE 2022.xlsx", 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric")) 

seguranca_publica_2021 <-
  read_excel("BancoVDE 2021.xlsx", 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric")) 

seguranca_publica_2020 <-
  read_excel("BancoVDE 2020.xlsx", 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric")) 


seguranca_publica_20 <-
  read_excel("BancoVDE 2020.xlsx", 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric")) 
ano<- 2024
ifelse(ano==2024,6,ifelse(between(ano, 2021, 2023),3,2))


fab<-
map_dfr(2024:2015, function(ano){
  url<- paste0("https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/download/dnsp-base-de-dados/bancovde-",ano,"-", ifelse(ano==2024,6,ifelse(between(ano, 2021, 2023),3,2)), ".xlsx/@@download/file
")
  print(url)
  arquivo<- paste0("bd_seguranca",ano,".xlsx")
  
  print(arquivo)
  download.file(url = url, destfile = arquivo , mode = "wb")
  read_excel(arquivo, 
             col_types = c("text", "text", "text", 
                           "date", "text", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "text", "text", 
                           "numeric")) 
  
  
})



seguranca_publica <-
  bind_rows(seguranca_publica_2022, seguranca_publica_2023, seguranca_publica_2024)


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
  filter(total_vitimas > 0,
         between(month(data_referencia),1,8),
         evento != "Pessoa Localizada") %>%
  mutate(ano = year(data_referencia)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,genero )) %>%
  mutate(genero = reorder(genero, soma )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma, fill = genero)) +
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
  filter(total_vitimas > 0,
         between(month(data_referencia),1,8)) %>%
  mutate(ano = year(data_referencia)) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento, genero )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma, fill = genero)) +
  theme_light() +
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Vítimas por tipo de evento e gênero",
    fill = "Gênero",
    subitle= "Dados entre janeiro e agosto de cada ano",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  ) +
    facet_wrap(evento~., scales = "free_y")


seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,8),
         evento %in% c("Estupro", "Feminicídio") ) %>%
  mutate(ano = as.character(year(data_referencia))) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  filter(genero == "feminino") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Vítimas femininas em crimes com predominância em mulheres",
    subtitle =  "Dados entre janeiro e agosto de cada ano",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  )+
  facet_wrap(evento~., scales = "free_y")


seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12),
         evento %in% c("Estupro", "Feminicídio") ) %>%
  mutate(ano = as.character(year(data_referencia))) %>%
  filter(ano<="2023") %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  filter(genero == "feminino") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Vítimas femininas em crimes com predominância em mulheres",
    #subtitle =  "Dados entre janeiro e agosto de cada ano",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  )+
  facet_wrap(evento~., scales = "free_y")



var_feminicidio_2022_2024<- ((882/975)-1)*100
var_feminicidio_2023_2024<- ((882/953)-1)*100

var_estupro_2022_2024<- ((43986/44757)-1)*100
var_estupro_2023_2024<- ((43986/47888)-1)*100

seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12),
         evento %in% c("Estupro", "Feminicídio") ) %>%
  mutate(ano = as.character(year(data_referencia))) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento ))


seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12)) %>%
  mutate(ano = year(data_referencia)) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento, genero )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma, fill = genero)) +
  theme_light() +
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Vítimas por tipo de evento e gênero",
    fill = "Gênero",
    subitle= "Dados entre janeiro e agosto de cada ano",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  ) +
  facet_wrap(evento~., scales = "free_y")

seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12)) %>%
  mutate(ano = year(data_referencia)) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento, genero )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma, fill = genero)) +
  theme_light() +
  theme(
    legend.position = "bottom"
  )+
  labs(
    title = "Vítimas por tipo de evento e gênero",
    fill = "Gênero",
    subitle= "Dados entre janeiro e agosto de cada ano",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  ) +
  facet_wrap(evento~., scales = "free_y")


seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12)) %>%
  mutate(evento = fct_reorder(evento, total_vitimas, sum)) %>%
  mutate(ano = year(data_referencia)) %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE)/1000,
            .by=c(evento, genero )) %>%
  ggplot() +
  geom_col(aes(x= soma, y= evento, fill = genero)) +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  )+
  labs(
    title = "Vítimas por tipo de evento e gênero",
    fill = "Gênero",
    subtitle =  "Acumulado entre 2015 e 2023.",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="Número de ocorrências em milhares",
    y=""
  ) 


seguranca_publica %>%
  filter(total_vitimas > 0,
         between(month(data_referencia),1,12),
         evento %in% c("Estupro", "Feminicídio") ) %>%
  mutate(ano = as.character(year(data_referencia))) %>%
  filter(ano<="2023") %>%
  pivot_longer(cols = feminino:nao_informado, names_to = "genero", values_to = "quantidade") %>%
  filter(genero == "feminino") %>%
  summarise(soma = sum(quantidade, na.rm = TRUE),
            .by=c(ano,evento )) %>%
  ggplot() +
  geom_col(aes(x= ano, y= soma)) +
  theme_light() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Vítimas femininas em crimes com predominância em mulheres",
    caption = "Fonte: Ministério da Justiça. Elaboração: Fernando Barbalho",
    x="",
    y=""
  )+
  facet_wrap(evento~., scales = "free_y")
