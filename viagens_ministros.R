library(tidyverse)
library(colorspace)
library(geobr)
library(readxl)

#ETL

de_para_nomes <- read_excel("de_para_nomes.xlsx")

de_para_nomes$nome_longo[5] <-"ANDRE LUIZ CARVALHO RIBEIRO"

sedes<- geobr::read_municipal_seat()
estados<- geobr::read_state()

viagens_ministros <- read_csv("viagens_ministros.csv", 
                              col_types = cols(`Valor Total` = col_character()), 
                              locale = locale())

viagens_ministros <- janitor::clean_names(viagens_ministros)


ministros_lula <- read_csv("ministros_lula.csv")

ministros_lula <- janitor::clean_names(ministros_lula)

ministros_lula<- ministros_lula[-15,]

ministros_lula$nome[28]<- "Márcio França"

sort(ministros_lula$nome)

sort(unique(viagens_ministros$nome))

tibble(nome_curto = sort(ministros_lula$nome), nome_longo = c(sort(unique(viagens_ministros$nome)),NA)) %>%
  readr::write_csv("tabela_nomes.csv")

names(ministros_lula)[2] <- "nome_curto_original"

ministros_lula<-
ministros_lula %>%
  inner_join(de_para_nomes)


ministros_lula<-
ministros_lula %>%
  separate(partido, into = c("partido","uf"), sep = "/")


viagens_ministros<-
viagens_ministros %>%
  separate(destinos, into = c("cidade","uf_destino"), sep = "/", remove = FALSE)  


viagens_ministros %>%
  filter(situacao == "Realizada") %>%
  summarise(quantidade = n(),
            .by = destinos) %>%
  ungroup() %>%
  mutate(destinos = reorder(destinos, quantidade)) %>%
  slice_max(order_by = quantidade, n=20) %>%
  ggplot(aes(x=quantidade, y= destinos, fill = quantidade)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = quantidade), hjust= -0.1, size =2.8, color  = "white") +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )




sedes_sel<-
  viagens_ministros %>%
  filter(situacao == "Realizada",
         stringr::str_detect(destinos,"[,]", negate=TRUE)) %>%
  mutate(cidade = str_to_title(cidade)) %>%
  summarise(quantidade = n(),
            .by = c(cidade,uf)) %>%
  ungroup() %>%
  inner_join(
    sedes %>%
      mutate(cidade = name_muni,
             uf = abbrev_state)
  )


estados %>%
  ggplot()+
  geom_sf(fill= "black") +
  geom_sf(data = sedes_sel, aes(geometry = geom,fill= quantidade, size=quantidade, color=quantidade),  pch= 21) +
  theme_void() +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  scale_color_continuous_sequential(palette = "Heat 2") +
  theme_void() +
  theme(
    panel.grid = element_blank()
  )

  
top_6_cidades<-
  (viagens_ministros %>%
  filter(situacao == "Realizada") %>%
  summarise(quantidade = n(),
            .by = destinos) %>%
  ungroup() %>%
  slice_max(order_by = quantidade, n=6))$destinos
  

top_20_cidades<-
  (viagens_ministros %>%
     filter(situacao == "Realizada") %>%
     summarise(quantidade = n(),
               .by = destinos) %>%
     ungroup() %>%
     slice_max(order_by = quantidade, n=20))$destinos


viagens_ministros %>%
  filter(situacao == "Realizada",
         destinos %in% top_6_cidades) %>%
  summarise(quantidade = n(),
            .by = c(destinos,nome)) %>%
  ungroup() %>%
  mutate(destinos = fct_reorder(destinos, quantidade,sum),
         nome = fct_reorder(nome, quantidade, sum)) %>%
  ggplot(aes(x=quantidade, y= nome, fill = quantidade)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = quantidade), hjust= -0.1, size =2.8, color  = "white") +
  scale_fill_continuous_sequential(palette = "Heat 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  ) +
  facet_wrap(destinos ~.)

  



viagens_ministros %>%
  rename(nome_longo = nome) %>%
  inner_join(ministros_lula) %>%
  mutate(tipo_viagem = case_when(
    tipo_viagem == "Internacional" ~ "Internacional",
    str_detect(destinos, uf, negate = TRUE) ~ "Outros destinos",
    str_detect(destinos, uf) & str_count(destinos, "/") > 1 ~ "Misto",
    str_detect(destinos, uf) & str_count(destinos, "/") == 1 ~ "Residência eleitoral"
  ))%>%
  mutate(tipo_viagem = factor(tipo_viagem, levels= c("Internacional", "Residência eleitoral", "Misto","Outros destinos"))) %>%
  summarise( quantidade = n(),
             .by = c(nome_curto_original,tipo_viagem)) %>%
  mutate(nome_curto_original = fct_reorder(nome_curto_original, quantidade, sum )) %>%
  ggplot() +
  geom_col(aes(x= quantidade, y= nome_curto_original, fill= tipo_viagem)) +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )


viagens_ministros %>%
  rename(nome_longo = nome) %>%
  inner_join(ministros_lula) %>%
  mutate(tipo_viagem = case_when(
    tipo_viagem == "Internacional" ~ "Internacional",
    str_detect(destinos, uf, negate = TRUE) ~ "Outros destinos",
    str_detect(destinos, uf) & str_count(destinos, "/") > 1 ~ "Misto",
    str_detect(destinos, uf) & str_count(destinos, "/") == 1 ~ "Residência eleitoral"
  ))%>%
  mutate(tipo_viagem = factor(tipo_viagem, levels= c("Internacional", "Residência eleitoral", "Misto","Outros destinos"))) %>%
  summarise( quantidade = n(),
             .by = c(nome_curto_original,tipo_viagem)) %>%
  mutate(nome_curto_original = fct_reorder(nome_curto_original, quantidade, sum )) %>%
  ggplot() +
  geom_col(aes(x= quantidade, y= nome_curto_original, fill= tipo_viagem), position = "fill") +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )



viagens_ministros %>%
  filter(destinos %in% top_20_cidades) %>%
  rename(nome_longo = nome) %>%
  inner_join(ministros_lula) %>%
  mutate(tipo_viagem = case_when(
    tipo_viagem == "Internacional" ~ "Internacional",
    str_detect(destinos, uf, negate = TRUE) ~ "Outros destinos",
    str_detect(destinos, uf) & str_count(destinos, "/") > 1 ~ "Misto",
    str_detect(destinos, uf) & str_count(destinos, "/") == 1 ~ "Residência eleitoral"
  ))%>%
  mutate(tipo_viagem = factor(tipo_viagem, levels= c("Internacional", "Residência eleitoral", "Misto","Outros destinos"))) %>%
  summarise( quantidade = n(),
             .by = c(destinos,tipo_viagem)) %>%
  mutate(destinos = fct_reorder(destinos, quantidade, sum )) %>%
  ggplot() +
  geom_col(aes(x= quantidade, y= destinos, fill= tipo_viagem), position = "fill") +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black")
  )


