library(readxl)
deslocamentos_escola <- read_excel("Relatorio_DF_percentual-2021.xlsx", 
                                           sheet = "A47", skip = 1, n_max=34)





deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(local == "DF" ) %>%
  mutate(tipo_deslocamento = reorder(tipo_deslocamento, valor)) %>%
  ggplot(aes(x=valor, y=tipo_deslocamento )) +
  geom_col(fill="white")+
  geom_text(aes(label = paste0(round(valor, 1),"%")), 
            hjust = c(rep(1.2,5),rep(-0.1,4)), 
            size = 3, 
            color = c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Principal meio de transporte para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )


dados_grafico<-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "A pé") %>%
  filter(!is.na(valor))
  

cor<- ifelse(dados_grafico$local=="DF","red","white")

 dados_grafico%>%
  mutate(local = reorder(local, valor)) %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos a pé para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )

dados_grafico <-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "Automóvel") %>%
  filter(!is.na(valor))
  

cor<- ifelse(dados_grafico$local=="DF","red","white")

 dados_grafico%>%
  mutate(local = reorder(local, valor)) %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos por automóvel para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )

dados_grafico<-
  deslocamentos_escola %>%
  select(-Total)%>%
  rename(local = Local) %>%
  mutate_at(vars(-local), ~ifelse(. == "(***)", NA, .)) %>%
  mutate_at(vars(-local), ~as.numeric(.)) %>%
  pivot_longer(2:10, names_to = "tipo_deslocamento", values_to = "valor") %>%
  filter(tipo_deslocamento == "Ônibus") %>%
  filter(!is.na(valor)) %>%
  mutate(local = reorder(local, valor))
  
cor<- ifelse(dados_grafico$local=="DF","red","white")
 
dados_grafico %>%
  ggplot(aes(x=valor, y=local )) +
  geom_col(fill=cor)+
  geom_text(aes(label = paste0(round(valor, 1),"%")),
            hjust = 1,# c(rep(1.2,5),rep(-0.1,4)),
            vjust=+0.3,
            size = 3,
            color = "black")+ #c(rep("black",5),rep("white",4))) +
  theme_light() +
  theme(
    panel.background = element_rect(fill="black"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Deslocamentos por ônibus para escola no DF",
    subtitle = "Dados de 2021",
    x= "",
    y=""
  )




names(deslocamentos_escola)