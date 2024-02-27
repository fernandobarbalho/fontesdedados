library(readxl)
agua_rede <- read_excel("Tabela 6803.xlsx", 
                          skip = 5, n_max = 1)


agua_rede %>%
  pivot_longer(cols=1:NCOL(agua_rede),
               names_to = "tipo_rede",
               values_to = "abrangencia") %>%
  mutate(tipo_rede  = str_wrap(tipo_rede,20),
         tipo_rede = reorder(tipo_rede, abrangencia)) %>%
  ggplot(aes(x=abrangencia, y=  tipo_rede)) +
  geom_col() +
  geom_text(aes(label= paste0(abrangencia,"%")), size= 3, hjust= c(1,-0.1,-0.1), color = c("white", "black", "black"))+
  theme_light() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "Acesso à rede geral de distribuição de água em 2022",
    subtitle = "Percentual de domicílios no Brasil",
    x= "Abrangência de domicílios (%)",
    y= "",
    caption = "Fonte: IBGE. Elaboração própria"
  )



esgotamento_sanitario <- read_excel("Tabela 6805.xlsx", 
                          skip = 5, n_max = 1)


esgotamento_sanitario %>%
  pivot_longer(cols=1:NCOL(esgotamento_sanitario),
               names_to = "tipo_rede",
               values_to = "abrangencia") %>%
  mutate(tipo_rede  = str_wrap(tipo_rede,20),
         tipo_rede = reorder(tipo_rede, abrangencia)) %>%
  ggplot(aes(x=abrangencia, y=  tipo_rede)) +
  geom_col() +
  geom_text(aes(label= paste0(abrangencia,"%")), size= 3, hjust= c(1,rep(-0.1,6)), color = c("white", rep("black",6)))+
  theme_light() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(
    title = "Acesso a esgotamento sanitário em 2022",
    subtitle = "Percentual de domicílios no Brasil",
    x= "Abrangência de domicílios (%)",
    y= "",
    caption = "Fonte: IBGE. Elaboração própria"
  )
