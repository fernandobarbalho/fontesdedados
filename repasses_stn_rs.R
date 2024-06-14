library(tabulizer)
library(tidyverse)

repasse1<-
tabulizer::extract_tables(file= "Comunicado AFM RS 2024.pdf")

repasse2<-
  tabulizer::extract_tables(file= "Comunicado AFM RS 2024 2.pdf")  

df_repasse1<-
as_tibble(repasse1[[1]]) %>%
  select(V2, V3) %>%
  mutate(V3= str_replace_all(V3, "[.]",""),
         V3= str_replace_all(V3, ",","."),
         V3 = as.numeric(V3)) %>% 
  bind_rows(
    as_tibble(repasse1[[2]]) %>%
      select(V2, V3) %>%
      mutate(V3= str_replace_all(V3, "[.]",""),
             V3= str_replace_all(V3, ",","."),
             V3 = as.numeric(V3))
  )


df_repasse1<- df_repasse1[-1,]

df_repasse2<-
  as_tibble(repasse2[[1]]) %>%
  select(V2, V4) %>%
  mutate(V4= str_replace_all(V4, "[.]",""),
         V4= str_replace_all(V4, ",","."),
         V4 = as.numeric(V4)) %>% 
  rename(V3= V4) %>%
  bind_rows(
    as_tibble(repasse2[[2]]) %>%
      select(V2, V3) %>%
      mutate(V3= str_replace_all(V3, "[.]",""),
             V3= str_replace_all(V3, ",","."),
             V3 = as.numeric(V3))
  )

df_repasse2<- df_repasse2[-1,]


df_repasse1$num_repasse <- 1
df_repasse2$num_repasse <- 2


df_repasse_total<- 
  df_repasse1 %>%
  bind_rows(df_repasse2)

names(df_repasse_total) <- c("municipio", "valor_repasse", "num_repasse")

#Geração de arquivo com nome de cidades 
df_repasse_total %>%
  distinct(municipio) %>%
  mutate(uf = "RS") %>%