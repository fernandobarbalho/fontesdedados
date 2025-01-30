library(tidyverse)

##Gráficos
library(sf)
library(spData)
library(colorspace)

dados_receitas_grants<- read_csv("GFSR_01-30-2025 11-30-35-94_timeSeries/GFSR_01-30-2025 11-30-35-94_timeSeries.csv")
View(GFSR_01_30_2025_11_30_35_94_timeSeries)

de_para_nome_codigo_pais <- read_csv("de_para_nome_codigo_pais.csv")

# dados_despesas_grants_trabalho %>% 
#   distinct(country_name) %>%
#   readr::write_csv("country_list.csv")


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
  filter(classification_name == "Grants expense") %>%
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

dados_despesas_grants_trabalho %>%
  inner_join(
    dados_pib_ppp_trabalho %>%
      select(iso_three_letters_code, gdp_ppp)
  )


data("world")
  
  