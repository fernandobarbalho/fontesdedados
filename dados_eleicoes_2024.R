library(readr)
library(tidyverse)
library(geobr)
library(sf)

estados_sf<- geobr::read_state()

resultado_eleicao_2024 <- read_delim("20241007_151828_eleicao24_prefeitos_vereadores_finalizados.csv", 
                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

glimpse(resultado_eleicao_2024)

NROW(
  resultado_eleicao_2024 %>%
    filter(cargo == "Prefeito") %>%
    distinct(cd_municipio_ibge)
)


num_municipios_estado<-
  resultado_eleicao_2024 %>%
  distinct(uf, cd_municipio_ibge ) %>%
  summarise(num_municipios = n(),
            .by = uf )



partidos_filtro<- c("PSD", "MDB", "PP", "UNIÃO", "PL", "REPUBLICANOS", "PSB", "PSDB", "PT" )

dados_grafico<-
  resultado_eleicao_2024 %>%
  filter(cargo == "Prefeito",
         situacao_candidato_turno == "Eleito") %>%
  summarise( quantidade = n(),
             .by =c(sg_partido, uf) ) %>%
  inner_join(num_municipios_estado) %>%
  mutate(percentual_eleito = (quantidade/num_municipios)*100) %>%
  filter(sg_partido %in% partidos_filtro) %>% 
  inner_join(
    estados_sf %>%
      rename(uf = abbrev_state)
  ) %>%
  mutate(percentual_eleito = ifelse(is.na(percentual_eleito),0, percentual_eleito))
  
  
dados_grafico %>%
  ggplot() +
  geom_sf( aes(fill= percentual_eleito, geometry = geom)) +
  facet_wrap(sg_partido ~.) +
  theme_void()
  
  
  

  
