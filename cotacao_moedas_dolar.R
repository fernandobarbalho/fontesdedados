library(readr)
library(tidyverse)

cotacao_moedas_dolar <- 
  read_csv("cotacao_moedas_dolar.csv")  %>%
  janitor::clean_names() %>%
  select(country, x2024_m12:x2025_m09) %>%
  pivot_longer(cols = x2024_m12:x2025_m09,
               names_to = "data",
               values_to = "cotacao",
               names_prefix = "x") %>%
  mutate(data= str_replace_all(data,"_m","-"),
         data = paste0(data, "-01"),
         data = as.Date(data)) %>%
  bind_rows( tibble(country = "Argentina", data = as.Date("2025-09-01"), cotacao= 0.0007246377))  %>%
  filter(!is.na(cotacao ))

cotacao_100<-
  cotacao_moedas_dolar %>%
  filter(data == "2024-12-01") %>%
  mutate(cotacao_100 =cotacao) %>%
  select(country, cotacao_100)

cotacao_indice<-
  cotacao_moedas_dolar %>%
  inner_join(cotacao_100) %>%
  mutate(variacao_referencia = ((cotacao/cotacao_100)-1)*100)

cotacao_indice_media <-
  cotacao_indice %>%
  summarise(variacao_referencia = mean(variacao_referencia, na.rm=TRUE),
            .by = c(data)) %>%
  mutate(country = "Média")

dados_grafico<- 
  cotacao_indice %>%
  mutate(country = case_when(
    str_detect(country, "Rus") ~ "Rússia",
    str_detect(country, "Bra") ~ "Brasil",
    .default = country
  ))

paises_sel <- c("Brasil", "Argentina","Rússia") 

dados_grafico_sel <-
  dados_grafico %>%
  filter(country %in% paises_sel)

dados_grafico_sel_ultimo_mes <-
  dados_grafico_sel %>%
  filter(data == "2025-09-01")

variação_brasil<-
  (dados_grafico_sel %>%
  filter(data == "2025-09-01",
         country == "Brasil"))$variacao_referencia

  

dados_grafico %>%
  filter(data ==as.Date("2025-09-01")  ) %>%
  arrange(desc(variacao_referencia))

datas_maximas<- 
dados_grafico %>%
  summarise(data = max(data),
            .by = country)

numero_acima<-
NROW(dados_grafico %>%
       inner_join(datas_maximas) %>%
       filter(variacao_referencia>0))

numero_abaixo<-
NROW(dados_grafico %>%
       inner_join(datas_maximas) %>%
       filter(variacao_referencia<0))



cor_fundo <- "#F8F8F8"

dados_grafico %>%
  ggplot(aes(x= data, y= variacao_referencia)) +
  geom_line(aes(group = country),color = "#DCDCDC") +
  geom_line(data = dados_grafico_sel, aes(colour = country), size = 1, show.legend = FALSE) +
  geom_line(data= cotacao_indice_media, linetype = "dashed", color = "#7C9937", size = 1 ) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  geom_text(data = dados_grafico_sel_ultimo_mes, 
            aes(label = paste0(" ",country, " ", round(variacao_referencia,0), "%"),
                color = country),
            show.legend = FALSE,
            hjust = 0) +
  geom_text(data = tibble(data = as.Date("2025-03-01"), variacao_referencia=33),
            aes(label =  str_wrap(paste0(numero_acima, " moedas com variação positiva"),15) ),
            hjust = 0.5) +
  geom_text(data = tibble(data = as.Date("2025-03-01"), variacao_referencia=-20),
            aes(label =  str_wrap(paste0(numero_abaixo, " moedas com variação negativa"),15) ),
            hjust = 0.5) +
  
  scale_color_manual(values = c("#74ACDF", "#FFCC01", "#D52B1E")) +
  scale_y_continuous(breaks = 0) +
  scale_x_date(breaks = c( as.Date("2024-12-01"),  as.Date("2025-09-01")),
               labels = scales::date_format(format = "%m/%Y"),
               expand = expansion(mult = c(0, 0.3))) +
  theme_light() +
  theme(
    text = element_text(family = "Noto Sans", colour = "#324e5a", size =10),
    panel.grid = element_blank(),
    plot.background = element_rect(fill =cor_fundo ),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  ) +
  labs(
    x= "",
    y= "",
    title = paste0("Real: quarta maior valorização ante o dólar em 2025"),
    subtitle = paste0("A moeda subiu ", round(variação_brasil,0) , "%"),
    caption = "Fontes: FMI e Banco Central da Argentina. Elaboração: VPR/DIESO"
  )
  

  