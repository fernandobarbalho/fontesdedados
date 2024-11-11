library(sidrar)
library(tidyverse)
library(geobr)
library(sf)
library(colorspace)

############### Número de Favelas por cidade brasileira

info_sidra(9883, wb = TRUE)

favelas_municipios<-  
  get_sidra(x = 9883,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 9910,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            #classific = "C544",
            #category =  list(c(129314 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_trabalho<-
  favelas_municipios %>%
  select(c(4:6)) %>%
  rename(
    quantidade = V,
    cod_ibge = D1C,
    municipio = D1N
  )

mapa_estados<- geobr::read_state()

mapa_municipios<- geobr::read_municipality()

mapa_municipios_seat<- geobr::read_municipal_seat()


mapas_favela<-
  mapa_municipios %>%
  inner_join(
    favelas_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge))
  )

favelas_trabalho<-
  favelas_trabalho %>%
  arrange(desc(quantidade))

numero_total<- sum(favelas_trabalho$quantidade)

favelas_trabalho$total_acumulado<- cumsum(favelas_trabalho$quantidade)

favelas_trabalho <-
  favelas_trabalho %>%
  mutate(code_muni= as.numeric(cod_ibge),
         perc_acumulado = total_acumulado/numero_total) 


mapas_favela_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapas_favela_seat %>%
  filter(perc_acumulado <=0.80) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(size = quantidade, fill = quantidade), pch = 21 ,color = "black" ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )

mapas_favela_seat %>%
  slice_max(order_by = quantidade, n=20) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = quantidade), pch = 21 ,color = "black" ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


mapas_favela %>%
  slice_max(order_by = quantidade, n=20) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(fill= quantidade)) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


### Proporção de habitantes nas favelas por cor/raça

info_sidra(9884, wb = TRUE)


favelas_municipios_cor<-  
  get_sidra(x = 9884,
            #variable = c(11601,1607,11602), #12607 (número índice com ajustes sazonal), 11601 mês/mês anterior com ajustes sazonal, 11602 mês/mesmo mês do ano anterior 
            variable = 1009612,
            #period = c("202301-202406"),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            classific = c("C86"),
            category =  list(c(2776, 2777,2778,2779, 2780,  2781     )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

favelas_cor_trabalho<-
  favelas_municipios_cor %>%
  select(c(4:6,9)) %>%
  rename(
    percentual = V,
    cod_ibge = D1C,
    municipio = D1N,
    cor_raca = D4N
  )

mapas_favela_cor_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_cor_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge)) 
  )


mapas_favela_cor_seat %>%
  filter(percentual >=10) %>%
  mutate(cor_raca = factor(cor_raca, levels = c("Indígena", "Preta", "Branca","Parda"))) %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = percentual), pch = 21 ,color = "black", size = 0.7 ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(color = "white")
  ) +
  facet_wrap(cor_raca~.) +
  labs(
    title =  "Cor/raça nas favelas brasileiras",
    subtitle =  "Dados do Censo de 2022",
    fill = "%",
    caption = "Fonte: IBGE. Elaboração: Fernando Barbalho"
  )

ggsave(filename = "cor_raca_favela.jpg")
ggsave(filename = "cor_raca_favela.png")
