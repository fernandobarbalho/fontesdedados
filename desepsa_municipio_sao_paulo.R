library(tidyverse)
library(colorspace)
desp_funcao_sao_paulo_capital <- read_csv("desp_funcao_sao_paulo_capital.csv")

desp_funcao_sao_paulo_capital <- janitor::clean_names(desp_funcao_sao_paulo_capital)

total_despesa <- sum(desp_funcao_sao_paulo_capital$value)

desp_funcao_sao_paulo_capital %>%
  summarise(total = sum(value),
            perc = round((total/total_despesa)*100,1),
            .by = funcao) %>%
  mutate(funcao = reorder(funcao, total),
         destaque = ifelse(funcao %in% c("Educação", "Segurança Pública"),"1","0")) %>%
  ggplot(aes(x=perc, y=funcao)) +
  geom_col(aes(fill = destaque), show.legend = FALSE) +
  geom_text(aes(x=perc+1.2, y= funcao, label = paste0(perc,"%"), color= destaque ), 
            size =3,
            fontface="bold", 
            show.legend = FALSE) +
  geom_text(aes(x=15, y= "Administração", label = str_wrap("A despesa com educação é quase 18x maior do que com segurança pública",30)),
            color= "white",
            size =3,
            #fontface="bold", 
            show.legend = FALSE) +
  
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  scale_color_discrete_qualitative(palette = "Dark 2") +
  theme_light()+
  annotate("curve", 
           y = "Habitação", 
           yend = "Educação", 
           x = 22, 
           xend = 22.2, 
           arrow = arrow(length = unit(0.3, "cm")),  # Adiciona a seta
           color = "white", size = 1)+
  annotate("curve", 
           y = "Desporto e Lazer", 
           yend = "Segurança Pública", 
           x = 15, 
           xend = 3.3, 
           arrow = arrow(length = unit(0.3, "cm")),  # Adiciona a seta
           color = "white",
           curvature = -0.3,
           size = 0.9)+
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    title = "Despesa por função em 2023",
    subtitle = "Município de São Paulo-SP",
    x= "",
    y="",
    caption = "Fonte: STN. Elaboração: Fernando Barbalho"
    
  )
  
  

