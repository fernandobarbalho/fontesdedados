library(tidyverse)
library(ggrepel)
library(lubridate)
library(caret)
library(rattle)

capag_sexo <- read_csv("capag_sexo.csv")

capag_sexo$saldo <- capag_sexo$saldo/1000
    

feminino_maior<-
  capag_sexo %>%
  filter(sexo %in% c(1, 3)) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  pivot_wider(names_from = sexo,
              values_from = saldo) %>%
  filter(abs(Feminino)>abs(Masculino),
         Feminino>0 & Masculino>0) %>%
  pivot_longer(cols = 3:4,
               names_to = "sexo",
               values_to = "saldo") %>%
  mutate(data_informacao = paste(ano, mes, "01", sep = "-"),
         data_informacao = ymd(data_informacao))

datas_sel<-
  feminino_maior %>%
  summarise(saldo = max(saldo),
            .by = data_informacao)


# Preparar os dados e criar o gráfico
capag_sexo %>%
  filter(sexo %in% c(1, 3)) %>%
  mutate(data_informacao = paste(ano, mes, "01", sep = "-"),
         data_informacao = ymd(data_informacao)) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  ggplot(aes(x = data_informacao, y = saldo)) +
  geom_line(aes(group = sexo, color = sexo)) +
  geom_point(data = feminino_maior, aes(color = sexo))+ 
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Ajustar para mostrar todos os meses
  theme_light() +
  geom_label_repel(data= datas_sel, 
                  aes(label = paste(as.character(month(data_informacao)), as.character(year(data_informacao)),sep="/")), 
                  size=3) +
  theme(panel.grid = element_blank()) +
  labs(
    x= "",
    y= "saldo E-D em milhares"
  )


capag_sexo_cnae <- read_csv("capag_sexo_cnae.csv")


# feminino_maior_cnae<-
#   capag_sexo_cnae  %>%
#   filter(sexo %in% c(1, 3)) %>%
#   filter(mes %in% c(2,11)) %>%
#   mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
#   pivot_wider(names_from = sexo,
#               values_from = saldo) %>%
#   filter(abs(Feminino)>abs(Masculino),
#          Feminino>0 & Masculino>0) %>%
#   mutate(saldo_pro_f = Feminino- Masculino) %>%
#   pivot_longer(cols = 4:5,
#                names_to = "sexo",
#                values_to = "saldo") %>%
#   mutate(data_informacao = paste(ano, mes, "01", sep = "-"),
#          data_informacao = ymd(data_informacao))


feminino_maior_cnae<-
capag_sexo_cnae  %>%
  filter(sexo %in% c(1, 3)) %>%
  filter(mes %in% c(2,11)) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  pivot_wider(names_from = sexo,
              values_from = saldo) %>%
  filter(abs(Feminino)>abs(Masculino),
         Feminino>0 & Masculino>0) %>%
  mutate(saldo_pro_f = Feminino- Masculino,
         prop_saldo_pro_f = saldo_pro_f/Feminino) %>%
  arrange(desc(saldo_pro_f))

feminino_maior_cnae %>%
  mutate(mes= as.factor(mes),
         ano = as.factor(ano),
         cnae_2_secao = fct_reorder(cnae_2_secao, saldo_pro_f, sum)) %>%
  ggplot(aes(x=saldo_pro_f, y=cnae_2_secao)) +
  geom_col() +
  facet_wrap(ano+mes~., ncol = 2)



fab<-
  capag_sexo_cnae %>%
  filter(sexo %in% c(1, 3),
         cnae_2_secao %in% c("P","G")) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  pivot_wider(names_from = sexo,
              values_from = saldo) %>%
  #  filter(abs(Feminino)>abs(Masculino),
  #         Feminino>0 & Masculino>0) %>%
  mutate(saldo_pro_f = Feminino- Masculino)


capag_sexo_cnae %>%
  filter(sexo %in% c(1, 3),
         cnae_2_secao %in% c("P","G","I","Q")) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  pivot_wider(names_from = sexo,
              values_from = saldo) %>%
  mutate(saldo_pro_f = Feminino- Masculino) %>%
  mutate(mes= as.factor(mes),
         ano = as.factor(ano),
         cnae_2_secao = fct_reorder(cnae_2_secao, saldo_pro_f, sum),
         sexo_preponderante = ifelse(saldo_pro_f>0,"Feminino","Masculino")) %>%
  ggplot(aes(x=saldo_pro_f, y=cnae_2_secao)) +
  geom_col(aes(fill=sexo_preponderante) ) +
  facet_wrap(ano+mes~., ncol = 12)




dados_modelo<-
  capag_sexo_cnae  %>%
  filter(sexo %in% c(1, 3)) %>%
  filter(mes %in% c(2,11)) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino")) %>%
  pivot_wider(names_from = sexo,
              values_from = saldo) %>%
  filter(Feminino>0 & Masculino>0) %>%
  mutate(saldo_pro_f = Feminino- Masculino,
         prop_saldo_pro_f = saldo_pro_f/Feminino,
         sexo_contratacao = ifelse(saldo_pro_f>0,"Feminino","Masculino")) %>%
  arrange(desc(saldo_pro_f))


modelo_lm<-
lm(saldo_pro_f~ as.factor(mes)+cnae_2_secao, data= dados_modelo )

summary(modelo_lm)

control_dt <- trainControl(method="cv")
seed <- 1972
set.seed(seed)



dt_model <- train(sexo_contratacao~ as.factor(mes)+cnae_2_secao,
                  data=dados_modelo, 
                  method="rpart", 
                  trControl=control_dt)

fancyRpartPlot(dt_model$finalModel)


capag_sexo_cnae %>%
  filter(sexo %in% c(1, 3),
         cnae_2_secao %in% c("P","G","I","Q")) %>%
  mutate(sexo = ifelse(sexo == 1, "Masculino", "Feminino"),
         mes = as.factor(mes)) %>%
  summarise(soma_saldo = sum(saldo),
            .by = c(mes,sexo,cnae_2_secao)) %>%
  ggplot(aes(x=mes, y= soma_saldo)) +
  geom_col(aes(fill= sexo), position = "dodge") +
  facet_wrap(cnae_2_secao~.)



