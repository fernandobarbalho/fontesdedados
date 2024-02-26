library(readr)
dados_ensino_medio_profissional <- read_csv("NATMON_DS_26022024144440528.csv")

glimpse(dados_ensino_medio_profissional)


dados_ensino_medio_profissional %>%
  distinct(NATMON_IND, Indicator )

# Países da América do Sul
america_sul <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", "PER", "SUR", "URY", "VEN")

# Países BRICS
brics <- c("BRA", "RUS", "IND", "CHN", "ZAF")

# Países da OCDE
ocde <- c("AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

# Combina todos os países
paises_selecionados <- unique(c(america_sul, brics, ocde))

# Imprime o vetor
print(todos_paises)


dados_brasil<- 
  dados_ensino_medio_profissional %>%
  filter(natmon_ind == "GTVP_2T3_V", 
         time == 2021,
         location == "BRA")

dados_ensino_medio_profissional <- janitor::clean_names(dados_ensino_medio_profissional)

dados_ensino_medio_profissional %>%
  filter(natmon_ind == "GTVP_2T3_V", 
         time == 2021,
         !is.na(value),
         location %in% paises_selecionados) %>%
  mutate(location = reorder(location,value)) %>%
  ggplot(aes(x=location,y=value)) +
  geom_col(fill= "blue") +
  geom_col(data = dados_brasil, fill="orange")+
  geom_text(data = dados_brasil,aes(label=paste0(round(value,1),"%")), vjust= -0.5, size=3 ) +
  theme_light()+
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "% de alunos de ensino médio em cursos vocacionais",
    subtitle = "Países: OCDE, América do Sul e BRICS",
    caption = "Fonte: Unesco. Dados de 2021. Elaboração própria",
    x= "",
    y=""
  )



dados_ensino_medio_profissional %>%
  filter(natmon_ind == "GTVP_2T3_V", 
         time == 2021,
         !is.na(value)) %>% 
  anti_join(dados_brasil) %>%
  ggplot(aes(x=natmon_ind,y=value)) +
  geom_boxplot(fill=NA, outlier.shape = NA)+
  geom_jitter(pch=21, fill= "blue",color="white") +
  geom_label(data = dados_brasil,aes(label=paste0("Brasil ",round(value,1),"%")), vjust= -0.5, size=3, color = "red" ) +
  geom_point(data = dados_brasil, pch=21, fill="red",color="white", size=2)+
  theme_light()+
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    title = "% de alunos de ensino médio em cursos vocacionais",
    subtitle = "Todos países da base",
    caption = "Fonte: Unesco. Dados de 2021. Elaboração própria",
    x= "",
    y=""
  )
