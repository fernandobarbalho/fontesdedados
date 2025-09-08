library(tidyverse)

relatorio_juros_acumulados <- read_delim("CopyOfRelatorioTXJurosAcumulados.csv", 
                                         delim = ";", escape_double = FALSE, col_types = cols(percentil25 = col_number(), 
                                                                                              percentil50 = col_number(), percentil75 = col_number(), 
                                                                                              percentil99 = col_number()), trim_ws = TRUE)
relatorio_juros_acumulados <-
  relatorio_juros_acumulados %>%
  mutate(
    percentil25 = percentil25/100,
    percentil50 = percentil50/100,
    percentil75 = percentil75/100,
    percentil99 = percentil99/100
  )


relatorio_juros_acumulados %>%
  filter(percentil99 > 100) %>%
  mutate(nomeIF = reorder(nomeIF,percentil99 )) %>%
  ggplot(aes(y= nomeIF, x= percentil99)) +
  geom_col() +
  geom_text(aes(x=percentil99+10,label= paste0( round(percentil99,1), "%"), ), hjust = 0) +
  scale_x_continuous(expand= expansion( mult  = c(0.1, 0.15)) ) +
  theme_light() +
  theme(
    plot.title = element_text(size=14, face = "bold"),
    plot.subtitle = element_text(size=12, face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    title= "Bancos que extrapolaram teto de taxas de juros no cartão",
    subtitle = "Julho de 2025",
    x= "",
    y=""
  )

