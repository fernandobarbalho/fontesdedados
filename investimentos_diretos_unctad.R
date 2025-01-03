library(tidyverse)



investimentos_diretos <- read_csv("US.FdiFlowsStock_20250103_180817.csv")

investimentos_diretos <- janitor::clean_names(investimentos_diretos)



investimentos_diretos_trabalho<- investimentos_diretos[239:7222,]

top_10<-
  investimentos_diretos_trabalho %>%
  filter(year == 2023) %>%
  slice_max(order_by = us_at_current_prices_in_millions_value, n=10)


top_10 %>%
  readr::write_csv("dez_maiores_ied_2023.csv")
