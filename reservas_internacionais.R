library(readr)
library(tidyverse)

reservas_internacionais <-  read_delim("STI-20251003144421575.csv", 
                                       delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                                                            `13621 - International reserves - Total - daily - US$ (million)` = col_number()), 
                                       trim_ws = TRUE)

names(reservas_internacionais)<- c("data", "valor") 


valor_inicio_governo<-
  (reservas_internacionais %>%
  filter(data == "2023-01-02"))$valor

valor_mil_dias<-
  (reservas_internacionais %>%
     filter(data == "2025-09-25"))$valor

cor_comparacao<- "orange"

cor_inicio<- "darkgreen"
cor_mil_dias<- "#0055A4"

variacao = (valor_mil_dias - valor_inicio_governo)/1000

variacao_perc = ((valor_mil_dias/valor_inicio_governo) -1)*100

cor_fundo<- "#F8F8F8"
      
reservas_internacionais %>%
  mutate(valor = valor/1000) %>%
  ggplot(aes(x=data, y= valor)) +
  geom_line() +
  geom_hline(yintercept = valor_inicio_governo/1000, color = cor_inicio, size= 1, linetype = "dashed") +
  geom_hline(yintercept = valor_mil_dias/1000, color = cor_mil_dias,size = 1, linetype = "dashed") +
  geom_segment(aes(x=as.Date("2025-09-25"), y= valor_inicio_governo/1000, xend = as.Date("2025-09-25"), yend = valor_mil_dias/1000 ),
               color = cor_comparacao, 
               size  = 1, 
               linetype = "dashed" ) +
  theme_light() +
  scale_y_continuous(breaks = c(round(valor_inicio_governo/1000,1), round(valor_mil_dias/1000,1) )) +
  scale_x_date(breaks = c(as.Date("2023-01-02"), as.Date("2025-09-25")), labels =  scales::date_format("%d/%m/%Y")) +
  geom_curve(
    aes(x = as.Date("2023-01-01"), y = valor_inicio_governo/1000, xend = as.Date("2023-06-01"), yend = valor_inicio_governo/1000 + 5),
    curvature = -0.1, ncp = 50,
    arrow = arrow(length = unit(4, "mm"), type = "closed"),
    color = cor_inicio
  ) +
  geom_curve(
    aes(x = as.Date("2025-09-25"), y = valor_mil_dias /1000, xend = as.Date("2025-06-01"), yend = valor_mil_dias/1000 + 5),
    curvature = +0.1, ncp = 50,
    arrow = arrow(length = unit(4, "mm"), type = "closed"),
    color = cor_mil_dias
  ) +
  geom_curve(
    aes(x = as.Date("2025-09-25"), y = 340, xend = as.Date("2025-06-01"), yend = 335),
    curvature = 0.4, ncp = 50,
    arrow = arrow(length = unit(4, "mm"), type = "closed"),
    color = cor_comparacao
  ) +
  geom_text(data = tibble(data = as.Date("2023-06-01")  , valor = valor_inicio_governo/1000 + 5), 
            aes(label = " Início goveno Lula"),
            color = cor_inicio,
            vjust = -0.1,
            hjust = 0,
            fontface = "bold")+
  geom_text(data = tibble(data = as.Date("2025-06-01")  , valor = valor_mil_dias/1000 + 6), 
            aes(label = "Mil dias de governo"),
            color = cor_mil_dias,
            vjust = -0.2,
            hjust = 0.5,
            fontface = "bold")+
  geom_label(data = tibble(data = as.Date("2025-06-01")  , valor = 335), 
            aes(label = str_wrap(paste0("Aumento de US$ ", round(variacao), " bi"),10) ),
            color = cor_comparacao,
            vjust = 1,
            hjust = 0.5,
            fontface = "bold")+
  theme(
    text = element_text(family = "Noto Sans", colour = "#324e5a", size =10),
    panel.grid = element_blank(),
    plot.background = element_rect(fill =cor_fundo ),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic")
  ) +
  labs(
    x= "",
    y= "Valores em US$ bi",
    title = paste0("Reservas internacionais crescem US$ ", round(variacao) , " bilhões no goveno Lula"),
    subtitle = paste0("Variação corresponde a aumento de ", round(variacao_perc,1) , "% na marca de mil dias de governo"),
    caption = "Fonte: BACEN. Elaboração: VPR/DIESO"
  )
  
ggsave(dpi = 300, filename = "reservas_internacionais.png")
      