library(readr)
library(colorspace)
library(ggbeeswarm)

arquivo<- "dataset_2025-09-19T18_32_30.559553248Z_DEFAULT_INTEGRATION_IMF.STA_GFS_COFOG_11.0.0.csv"

cofog_saude <- 
  read_csv(arquivo) %>%
  janitor::clean_names() %>%
  pivot_longer(cols = x2021:x2024,
               names_to = "ano",
               values_to = "valor",
               names_prefix = "x") %>%
  mutate(ano = as.numeric(ano))

saude_total<- "Health, Transactions"

paises_traduzidos <- read_csv("paises_traduzidos.csv")

paises_sel<-
  (cofog_saude %>%
     inner_join(paises_traduzidos) %>%
  filter(indicator == saude_total,
         ano == 2022) %>%
  slice_max(order_by = valor, n=40) %>%
  select(nome_pais, valor))$nome_pais




dados_grafico<-
  cofog_saude %>%
  inner_join(paises_traduzidos) %>%
  mutate(nome_pais = factor(nome_pais, levels= paises_sel)) %>%
  filter(nome_pais %in% paises_sel,
         ano == 2022,
         indicator != saude_total,
         !is.na(valor))
  


dados_grafico %>%
  ggplot() +
  geom_col(aes(x= nome_pais, y= valor, fill= indicator), position = "fill") +
  scale_fill_discrete_qualitative(palette = "Dark 3") +
  theme_light() +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust =0.5),
    axis.title.y  = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) 
  


dados_proporcao_gasto_saude<-
dados_grafico %>%
  inner_join(
    cofog_saude %>%
      filter(indicator == saude_total,
             country %in% paises_sel,
             ano == 2022) %>%
      mutate(total = valor) %>%
      select(country, total)
  ) %>%
  mutate(
    proporcao_gasto_saude = (valor/total)*100
  )
  


dados_proporcao_gasto_saude %>%
  summarise( media = mean(proporcao_gasto_saude),
             minino = min(proporcao_gasto_saude), 
            primeiro_quartil = quantile(proporcao_gasto_saude)[2],
            mediana = median(proporcao_gasto_saude),
            terceiro_quartil = quantile(proporcao_gasto_saude)[4],
            maximo = max(proporcao_gasto_saude),
            .by = indicator) %>%
  arrange(indicator)

brasil<-
dados_proporcao_gasto_saude %>%
  filter(country == "Brazil") %>%
  select(indicator, proporcao_gasto_saude)%>%
  arrange(indicator)

cor_fundo<- "#F8F8F8"

dados_proporcao_gasto_saude %>%
  ggplot(aes(x= indicator, y= proporcao_gasto_saude)) +
  geom_boxplot() +
  geom_beeswarm(fill= "gray", pch= 21, color = "black")+
  geom_point(data= brasil, fill= "red", pch= 21, color = "black", size= 2) +
  theme_light() +
  theme(
    panel.grid =  element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill =cor_fundo )
    ) +
  facet_wrap(indicator~., scales = c("free"))
  
