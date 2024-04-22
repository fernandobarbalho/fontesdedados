library(tidyverse)
library(colorspace)
library(geobr)

sedes<- geobr::read_municipal_seat()
estados<- geobr::read_state()

viagens_ministros <- read_csv("viagens_ministros.csv", 
                              col_types = cols(`Valor Total` = col_character()), 
                              locale = locale())

viagens_ministros <- janitor::clean_names(viagens_ministros)




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
  separate(destinos, into = c("cidade","uf"), sep = "/") %>%
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

  

