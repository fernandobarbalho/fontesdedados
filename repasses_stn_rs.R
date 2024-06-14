library(tabulizer)
library(tidyverse)

library(sf)
library(spData)
library(colorspace)


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
  readr::write_csv("municipios_repasse_rs.csv")

municipios_repasse_rs_ibge <- read_csv("municipios_repasse_rs_updated.csv")

saveRDS(municipios_repasse_rs_ibge,"municipios_repasse_rs_ibge.rds")




municipios<- geobr::read_municipality(simplified = FALSE)
estados<- geobr::read_state()




municipios_rs<-
  municipios %>%
  filter(abbrev_state == "RS")


argentina_uruguai_paraguay<-
  world %>%
  filter(iso_a2 %in% c("UY","AR","PY"))

limites <- data.frame(
  xmin = -58,
  xmax = -53,
  ymin = -31,
  ymax = -26
)


dados_grafico<-
  municipios %>%
  filter(abbrev_state == "RS") %>%
  inner_join(
    df_repasse_total %>%
      inner_join(municipios_repasse_rs_ibge) %>%
      summarise(total_repasse = sum(valor_repasse),
                .by = c(codigo_ibge, municipio)) %>%
      mutate(code_muni = as.numeric(codigo_ibge),
             valor_total = round(total_repasse/10^6)))
  

muni_sel <-
  dados_grafico %>%
  filter(code_muni %in% c("4314407","4314902","4316907")) #,""
#  slice_max(order_by = valor_total, n=5)


muni_sel <-
  dados_grafico %>%
  filter(code_muni %in% "vazio") #,""


dados_grafico %>%
  ggplot() +
  geom_rect(
    data = limites,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "#808080"
  )+
  geom_sf(data = argentina_uruguai_paraguay, fill= "#808080") +
  geom_sf(data = estados, fill= "#808080") +
  geom_sf(data = estados %>%filter(abbrev_state == "RS")) +
  geom_sf(data= municipios_rs, fill=NA, color = "#F5F5F5") +
  geom_sf( aes(fill=valor_total) )+
  geom_text(aes(x=-51, y=-27, label= "Santa Catarina")) +
  geom_text(aes(x=-56, y=-32, label= "Uruguai")) +
  geom_text(aes(x=-57, y=-28, label= "Argentina")) +
  geom_sf_text( data= muni_sel, 
                aes(label= str_wrap(paste(name_muni, valor_total, sep=":"),100)), 
                size=2,
                #hjust = c(0.1,-0.5,0.1),
                #vjust = c(0,0,1,0,0), 
                color = "black",
                fontface = "bold") +
  scale_fill_continuous_sequential(palette = "Heat 2" )+
  coord_sf(xlim = c(-58,-49), ylim=c(-33.5,-27))+
  theme_void() +
  theme(
    panel.background = element_rect(fill="#0077be")
  ) +
  labs(
    fill = "",
    title = "Apoio financeiro aos municípios do RS",
    subtitle = "Valores em R$ milhões",
    caption = "Fonte: Secretaria do Tesouro Nacional. Elaboração VPR/DIESO"
  ) 

