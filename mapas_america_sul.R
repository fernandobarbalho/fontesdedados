library(readxl)
library(geobr)
library(tidyverse)
library(sf)
library(spData)
library(ggrepel)
library(colorspace)

data("world")


#mapa mundi


world %>%
  ggplot() +
  geom_sf(aes(fill=pop/10^6)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("População em milhões de habitantes", 10)
  )


world %>%
  ggplot() +
  geom_sf(aes(fill=pop)) +
  scale_fill_continuous_sequential(palette= "Heat 2", trans= "log2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be"),
    legend.position = "none"
  )


total_pib_america_sul<-
  (world %>%
  filter(continent == "South America") %>%
  summarise(pib_continente =  sum(gdpPercap * pop, na.rm = TRUE)))$pib_continente




transformed_data <- read_csv("transformed_data.csv")

# Criar um vetor com os códigos de países
codigos_paises <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "PER", "PRY", "URY", "VEN")

# Tabela de correspondência entre códigos de países e códigos de duas letras ISO
tabela_iso <- data.frame(
  codigo_pais = c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "PER", "PRY", "URY", "VEN"),
  codigo_iso = c("AR", "BO", "BR", "CL", "CO", "EC", "PE", "PY", "UY", "VE"),
  stringsAsFactors = FALSE
)


unique(transformed_data$codigo_pais)

dados_pib<-
transformed_data %>%
  inner_join(tabela_iso) %>%
  mutate(iso_a2= codigo_iso) %>%
  filter(ano %in% c(1960, 2023)) %>%
  bind_rows(tibble( codigo_pais= "GUY", ano = 1960, percentual_pib= NA, iso_a2 ="GY")) %>%
  bind_rows(tibble( codigo_pais= "SUR", ano = 1960, percentual_pib= NA, iso_a2 ="SR")) %>%
  bind_rows(tibble( codigo_pais= "GUY", ano = 2023, percentual_pib= NA, iso_a2 ="GY")) %>%
  bind_rows(tibble( codigo_pais= "SUR", ano = 2023, percentual_pib= NA, iso_a2 ="SR")) 


southamerica<-
  world %>%
  filter(continent=="South America") 


france<-
  world %>%
  filter(iso_a2 == "FR")



southamerica$lon<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,1]   
southamerica$lat<- sf::st_coordinates(sf::st_centroid(southamerica$geom))[,2]




dados_textos<-
  southamerica%>%
  filter(continent == "South America",
         iso_a2 != "FK") %>%
  inner_join(
    dados_pib %>%
      filter(!is.na(percentual_pib)) 
  ) %>%  filter(ano %in% c(1960, 2023))


southamerica%>%
  filter(continent == "South America",
         iso_a2 != "FK") %>%
  left_join(
    dados_pib  
  ) %>%
  filter(!is.na(ano)) %>%
  #mutate(ano = 1960) %>%
  ggplot() +
  geom_sf(aes(fill=percentual_pib*100)) +
  geom_sf(data=france) +
  geom_label(data= dados_textos,aes(x=lon, y=lat,label= str_wrap(paste0(codigo_pais," ",round(percentual_pib*100,1),"%"),3)), 
                  color = "black", 
                  fontface = "bold", 
                  size = 2.5) +
  geom_label(data= dados_textos,aes(x=-50, y=-40,label= ano), 
             color = "black", 
             fontface = "bold", 
             size = 6) +

  scale_fill_continuous_sequential(palette= "Heat 2" )+
  coord_sf(xlim = c(-82,-35), ylim=c(-60,15))+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be"),
    strip.text = element_blank()
  ) +
  labs(
    title = "Participação no PIB gerado na América do Sul",
    subtitle = "Participação por país",
    fill= str_wrap("(%)", 10)
  ) +
  facet_wrap(ano~.)



