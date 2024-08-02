library(ipeadatar)
library(tidyverse)

series_disponiveis<-
ipeadatar::available_series()


codigos<-
((series_disponiveis %>%
  filter(str_detect(code, "^PRECOS12_IPCA[a-zA-Z]" )) %>%
  select(code)))$code

variacao<-
  ipeadatar::ipeadata(codigos)

variacao_trabalho<-
variacao %>%
  inner_join(series_disponiveis)
  
variacao_trabalho$variacao_acumulada <- 100

fab<-
variacao_trabalho %>%
  group_by(code) %>%
  mutate(lag_value = lag(value, n=1)) %>%
  mutate(variacao_acumulada =  ifelse(is.na(lag_value), 100 * (1+value/100), lag(variacao_acumulada, n=1) * (1+value/100)))
