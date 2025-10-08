library(readxl)
library(tidyverse)

gdp_per_capita_worldbank <- 
  read_excel("gdp-per-capita-worldbank.xlsx") %>%
  janitor::clean_names() %>%
  filter(year==2024) %>%
  rename(value = gdp_per_capita_ppp_constant_2021_international,
         regiao = world_regions_according_to_owid)


dados_grafico <-
  gdp_per_capita_worldbank %>%
  mutate(escopo = "mundo")

dados_grafico_brasil<-
  dados_grafico %>%
  filter(code == "BRA")

dados_grafico %>%
  ggplot(aes(x= escopo, y= value)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  geom_jitter(data = dados_grafico_brasil, color = "red")



dados_grafico %>%
  ggplot(aes(x= regiao, y= value)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter() +
  geom_jitter(data = dados_grafico_brasil, color = "red")
