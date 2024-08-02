library(tidyverse)

desemprego_brasil <- read_delim("20240628021609.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                              skip = 1)

desemprego_brasil_trabalho<-

desemprego_brasil %>% 
  pivot_longer(cols = -1,
               names_to = "periodo",
               values_to = "valor") %>%
  select(-1)
  

fab<-
  desemprego_brasil_trabalho %>%
  mutate(num_linha = as.character(row_number())) %>%
  mutate(tamanho_num_linha = str_length(num_linha)) %>%
  mutate(num_zero = 3- tamanho_num_linha ) 


 desemprego_brasil_trabalho %>%
  mutate(num_linha = as.character(row_number())) %>%
  mutate(tamanho_num_linha = str_length(num_linha)) %>%
  mutate(num_zero = 4- tamanho_num_linha )>%
  mutate(teste = rep("0", num_zero)) %>%
  mutate(periodo = paste( rep("0",4-str_length(num_linha)),  num_linha,periodo, sep = ":", collapse = "")) %>%
  ggplot(aes(x= periodo, y= valor)) +
  #geom_col() +
  geom_line(aes(group = 1)) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle= 90)
  )

str_c(rep(0,2), "oi")

resultado <- rep("0", 2)  # Isso cria um vetor com duas strings "0"
string_final <- paste(resultado, collapse = "")  # Concatena as strings sem espaços

print(string_final)


rep("0",3-str_length(as.character(200)) )


rep("0",-1)



