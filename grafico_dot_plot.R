library(tidyverse)
library(readxl)
dados_grafico_dot_plot <- 
  read_excel("Pasta1.xlsx") %>%
  janitor::clean_names()

names(dados_grafico_dot_plot)[1]<- "uf"

ordem<- sort(dados_grafico_dot_plot$x2021_2024, decreasing = TRUE)

dados_grafico_dot_plot %>%
  mutate(uf = reorder(uf, ordem)) %>%
  pivot_longer(x2016_2019:x2021_2024, names_to = "periodo", values_to = "valor") %>%
  ggplot(aes(x=valor, y=uf))+
  geom_point(aes(group = uf, color = periodo)) +
  geom_line(aes(group= uf), color ="blue")
  
  
  
  
  
  dados_grafico %>%
  ggplot(aes(x=pago_dotacao, y= orgao_descricao))+
  geom_point(aes(group = orgao_descricao, color = ano)) +
  geom_line(aes(group= orgao_descricao, color = diferenca_anos))+
  #scale_fill_discrete_divergingx(palette = "Zissou 1", rev = TRUE)+
  #scale_color_discrete_divergingx(palette = "Zissou 1", rev = TRUE)+
  scale_color_manual(values = c("red","blue","red","blue"))+
  theme_light()+
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill= "white"),
    axis.text.y = element_text(color =cor_texto_eixo)
  ) +
  labs(
    title = "Ranking: pagamentos totais/dotacao",
    subtitle = "Valores em (%). Despesas primárias Jan-Mai",
    caption = "Fonte: STN. Elaboração própria",
    color = "",
    x = "",
    y = ""
  )
  
