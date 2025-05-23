library(tidyverse)

##Gráficos
library(sf)
library(spData)
library(colorspace)

dados_receitas_grants<- read_csv("GFSR_01-30-2025 11-30-35-94_timeSeries/GFSR_01-30-2025 11-30-35-94_timeSeries.csv")
View(GFSR_01_30_2025_11_30_35_94_timeSeries)



de_para_nome_codigo_pais <- read_csv("de_para_nome_codigo_pais.csv")


dados_receitas_grants_trabalho<-
dados_receitas_grants %>%
  select(1:61) %>%
  pivot_longer(cols = 10:61, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  janitor::clean_names() %>%
  rename(imf_country_code = country_code) %>%
  filter(attribute == "Value",
         classification_name == "Grants revenue") %>%
  filter(year == 2021) %>%
  left_join(de_para_nome_codigo_pais) 


conta<- "Grants expense to foreign govts"

dados_despesas_grants<- read_csv("GFSE_01-30-2025 11-32-30-39_timeSeries/GFSE_01-30-2025 11-32-30-39_timeSeries.csv")

dados_despesas_grants_trabalho<-
  dados_despesas_grants %>%
  select(1:61) %>%
  pivot_longer(cols = 10:61, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  janitor::clean_names() %>%
  rename(imf_country_code = country_code) %>%
  filter(year == 2021) %>%
  filter(sector_name == "General government") %>%
  filter(attribute == "Value") %>%
  filter(classification_name == conta) %>%
  left_join(de_para_nome_codigo_pais)


dados_pib_ppp <- read_csv("API_NY.GDP.MKTP.PP.KD_DS2_en_csv_v2_5169.csv", 
                                                     skip = 3)

dados_pib_ppp_trabalho<-
  dados_pib_ppp %>%
  select(1:68) %>%
  pivot_longer(cols = 5:68, names_to = "year", values_to = "gdp_ppp") %>%
  janitor::clean_names() %>%
  rename(iso_three_letters_code = country_code) %>%
  filter(year == 2021)


despesas_ppp<-
dados_despesas_grants_trabalho %>%
  inner_join(
    dados_pib_ppp_trabalho %>%
      select(iso_three_letters_code, gdp_ppp)
  ) %>%
  mutate(despesa_uss_ppp = (value/100 * gdp_ppp))


data("world")




world %>%
  left_join(dados_receitas_grants_trabalho) %>%
  ggplot() +
  geom_sf(aes(fill= value)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("Proporção da receita vinda de grants em %", 10)
  ) 

world %>%
  left_join(dados_despesas_grants_trabalho) %>%
  ggplot() +
  geom_sf(aes(fill= value)) +
  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill= str_wrap("Proporção da despesa em grants em %", 10)
  ) 

dados_mapa_despesa_ppp<-
  world %>%
  left_join(despesas_ppp) %>%
  mutate(despesa_uss_ppp = despesa_uss_ppp/10^9)
  
paises_sel<-
  dados_mapa_despesa_ppp %>%
  filter(iso_three_letters_code %in% c("USA","BRA","ZAF"))


mapa<-
dados_mapa_despesa_ppp %>%
  ggplot() +
  geom_sf(aes(fill= despesa_uss_ppp)) +
  geom_sf_label(data = paises_sel, 
                aes(label = str_wrap(paste(iso_three_letters_code,"US$", round(despesa_uss_ppp,1),"bi"),20)),
                size= 2.8,
                alpha = 0.5,
                fontface = "bold"
               ) +
  geom_sf_text(x=-160,y=40, 
                aes(label= str_wrap("EUA respondem por 44,4% das doações a países estrangeiros *",35)),
                size= 2.8,
                alpha = 0.5,
               color = "white",
               fontface = "bold"
               )+
  geom_sf_text(x=-25,y=45, 
                aes(label= str_wrap("Alemanha e França: 60% das doações europeias *",15)),
                size= 2.8,
                alpha = 0.5,
               color = "white",
                fontface = "bold")+
  geom_sf_text(x=15,y=-40, 
                aes(label= str_wrap("África do Sul lidera entre os BRICS *",40)),
                size= 2.8,
                alpha = 0.5,
               color = "white",
                fontface = "bold")+
  geom_sf_text(x=155,y=25, 
                aes(label= str_wrap("Países asiáticos: média de doações = US$ 0,2 bi *",30)),
                size= 2.8,
                alpha = 0.5,
               color = "white",
                fontface = "bold")+

  scale_fill_continuous_sequential(palette= "Heat 2" )+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be"),
    legend.text = element_text(size = 7),      # Label size
    legend.title = element_text(size = 8),     # Title size
    legend.key.size = unit(0.5, "cm"),           # Key size
     plot.title = element_text(
      hjust = 0.5,
      size = 16,
      color = "darkblue",
      face = "bold")
  ) +
  coord_sf(xlim = c(-180,180), ylim=c(-60,90))+
  labs(
    title = "Doações a países estrangeiros em 2021",
    fill= str_wrap("US$ bi PPP", 10),
    caption = "* Considerando os países que forneceram dados ao FMI. Fonte: Banco Mundial e FMI. Elaboração: Fernando Barbalho"
  ) 

ggsave(filename = "mapa_doacoes.jpeg", plot= mapa, width = 13.3, height = 7.5)

#tamanho 1051 x 638

69.4/sum(dados_mapa_despesa_ppp$despesa_uss_ppp, na.rm=TRUE)

dados_mapa_despesa_ppp %>%
  filter(continent == "Europe") %>%
  summarise(sum(despesa_uss_ppp,na.rm = TRUE))

dados_mapa_despesa_ppp %>%
  filter(continent == "Asia") %>%
  summarise(mean(despesa_uss_ppp,na.rm = TRUE))

