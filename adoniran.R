library(colorspace)
set.seed(13)
motivos<- c("Teu olhar","Bala de carabina","Veneno estriquinina", "Peixeira de baiano", "Atropelamento de automóver", "Bala de revórver")
frequencia<-  c(200,sort(sample(60:100,5), decreasing = TRUE))

causas_mortis<-
  tibble(motivo = motivos, frequencia = frequencia)

causas_mortis %>%
  mutate(motivo = reorder(motivo, frequencia)) %>%
  ggplot(aes(x= frequencia, y= motivo)) +
  geom_col(aes(fill = frequencia), show.legend = FALSE) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_minimal(base_family = "Comic Sans MS") +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Causas mortis no Bixiga em 1960",
    x="",
    y="",
    caption= "Fonte: Adoniran Barbosa. Elaboração: Fernando Barbalho"
  )

ggsave(filename = "tiro_ao_alvaro.jpg", dpi=300, device = "jpeg")

library(ggplot2)
library(showtext)

# Adiciona suporte para texto com showtext
showtext_auto()

# Carrega a fonte Brush Script MT
font_add("Brush Script", "BrushScriptMT.ttf")

# Cria um gráfico com título estilizado
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  ggtitle("O Ritmo do Samba") +
  theme_minimal(base_family = "Brush Script")
