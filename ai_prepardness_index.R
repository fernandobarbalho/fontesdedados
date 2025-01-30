library(tidyverse)
library(readxl)



trata_tabela<- function(ds_name, index_name){
  ds <- read_excel(ds_name)
  
  ds<- ds[-1,]
  
  names(ds) <- c("pais","indice")
  
  ds$tipo_indice<- index_name
  
  ds<- filter(ds,!is.na(indice))
  
  ds
  
}
  



#trata índice de preparo para IA
indice_preparo_ia<- trata_tabela( "imf-dm-export-20250128.xls" , "preparo_ia" ) #"imf-dm-export-20250102.xls"


#trata índice de infraestrutura digital
infra_estrutura_digital<- trata_tabela("imf-dm-export-20250102(1).xls", "infraestrutura_digital")


#trata índice de capital humano
capital_humano<- trata_tabela("imf-dm-export-20250102(4).xls", "capital_humano")


#Importa índice de inovação e integração econômica
inovacao_integracao_ecomomica<- trata_tabela("imf-dm-export-20250102(2).xls", "inovacao_integracao_ecomomica")


#Importa índice de regulação e ética
regulacao_etica<- trata_tabela("imf-dm-export-20250102(3).xls", "regulacao_etica")



consolidado_preparo_ia<-
  bind_rows(
    indice_preparo_ia,
    infra_estrutura_digital,
    capital_humano,
    inovacao_integracao_ecomomica,
    regulacao_etica
  )


consolidado_preparo_ia %>%
  ggplot(aes(x= tipo_indice, y = indice)) +
  geom_boxplot()

consolidado_preparo_ia %>%
  readr::write_csv("dados_preparedness_to_ia.csv")

consolidado_preparo_ia %>%
  filter(pais == "Brazil")

dados_grafico<-
  indice_preparo_ia %>%
  filter(row_number()<=165) %>%
  mutate(pais= ifelse(pais=="China, People's Republic of","China",pais)) %>%
  mutate(pais = reorder(pais, desc(indice) ) ) %>%
  slice_max(order_by = indice, n=80)
  

dados_grafico %>%
  ggplot(aes(x = pais, y = indice, fill = pais == "China")) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = ifelse(pais %in% c("Singapore", "China"), round(indice,2), "")), 
            vjust = -0.5, 
            color = "black",
            size= 3) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray")) +
  scale_y_continuous(expand =  c(0,0.1))+
  theme_light() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.1)
  ) +
  labs(
    y = "AI Preparedness Index",
    x = "",
    title = str_wrap("Ranking de Preparação para a IA", 72),
    subtitle = "A Posição da China Segundo o AI Preparedness Index",
    caption = "Fonte: FMI (2023). Elaboração: Fernando Barbalho"
  )

ggsave(filename = "ranking_40_aipi.png",  units = "cm", width = 20, height = 9.04, dpi = 300)
