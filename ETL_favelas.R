library(sidrar)
library(tidyverse)
library(geobr)
library(sf)
library(colorspace)

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


mapas_favela_seat<-
  mapa_municipios_seat %>%
  inner_join(
    favelas_trabalho %>%
      mutate(code_muni= as.numeric(cod_ibge))
  )


mapas_favela_seat %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(size = quantidade, fill = quantidade), pch = 21 ,color = "black" ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )

mapas_favela_seat %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes( fill = quantidade), pch = 21 ,color = "black" ) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )


mapas_favela %>%
  ggplot() +
  geom_sf(data = mapa_estados, fill = NA) +
  geom_sf(aes(fill= quantidade)) +
  scale_fill_continuous_sequential(palette= "Heat 2") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black")
  )
