library(readr)
library(tidyverse)
library(geobr)
library(sf)
library(colorspace)

estados_sf<- geobr::read_state()

resultado_eleicao_2024 <- read_delim("20241007_151828_eleicao24_prefeitos_vereadores_finalizados.csv", 
                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

glimpse(resultado_eleicao_2024)

NROW(
  resultado_eleicao_2024 %>%
    filter(cargo == "Prefeito") %>%
    distinct(cd_municipio_ibge)
)


num_municipios_estado<-
  resultado_eleicao_2024 %>%
  distinct(uf, cd_municipio_ibge ) %>%
  summarise(num_municipios = n(),
            .by = uf )



partidos_filtro<- c("PSD", "MDB", "PP", "UNIÃO", "PL", "REPUBLICANOS", "PSB", "PSDB", "PT" )

dados_grafico<-
  resultado_eleicao_2024 %>%
  filter(cargo == "Prefeito",
         situacao_candidato_turno == "Eleito") %>%
  summarise( quantidade = n(),
             .by =c(sg_partido, uf) ) %>%
  mutate(sg_partido =fct_reorder(sg_partido, quantidade, sum, .desc = TRUE)) %>%
  inner_join(num_municipios_estado) %>%
  mutate(percentual_eleito = (quantidade/num_municipios)*100) %>%
  filter(sg_partido %in% partidos_filtro) %>% 
  inner_join(
    estados_sf %>%
      rename(uf = abbrev_state)
  ) %>%
  mutate(percentual_eleito = ifelse(is.na(percentual_eleito),0, percentual_eleito))
  

grafico<-  
dados_grafico %>%
  ggplot() +
  geom_sf( aes(fill= percentual_eleito, geometry = geom)) +
  scale_fill_continuous_sequential(palette = "Heat 2")+
  theme_void() +
  labs(
    title = "Proporção de prefeitos eleitos por estado e partido",
    subtitle = "Primeiro turno 2024",
    fill= "(%)",
    caption = "Fonte: TSE. Elaboração: Fernando Barbalho"
  ) +
  facet_wrap(sg_partido ~.) 
  
ggsave("resultados_prefeitos.png", plot = grafico, width = 10, height = 5, dpi = 300, type = "cairo-png")  
  


prefeitos_coligacao_pt_ou_psb <- 
  resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(PT|PSB)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito")  


prefeitos_coligacao_chapa_lula <- 
  resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(PT|PSB|PV|PSOL|REDE|AVANTE|AGIR|SOLIDARIEDADE)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito")  


prefeitos_coligacao_novo_pl <- 
  resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(NOVO|PL)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito")  



nordeste_pt_psb<-
resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(PT|PSB)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito",
         regiao == "NE")  


nordeste_pl_novo<-
  resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(PL|NOVO)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito",
         regiao == "NE")  




prefeitos_coligacao_pt <- 
  resultado_eleicao_2024 %>%
  filter(str_detect(membros_coligacao, "\\b(PT)\\b"),
         cargo == "Prefeito",
         situacao_candidato_turno == "Eleito")  


candidatos_prefeitos_resultado<-
  resultado_eleicao_2024 %>%
  filter(cargo == "Prefeito") %>%
  select(sg_partido, situacao_candidato_turno)

teste_chisq<-
chisq.test(candidatos_prefeitos_resultado$sg_partido, candidatos_prefeitos_resultado$situacao_candidato_turno,simulate.p.value = TRUE )


candidatos_prefeitos_resultado %>%
  filter(!is.na(situacao_candidato_turno)) %>%
  summarise(quantidade = n(),
            .by = c(sg_partido, situacao_candidato_turno)) %>%
  mutate(sg_partido = fct_reorder(sg_partido, quantidade,sum)) %>%
  ggplot(aes(x=quantidade, y = sg_partido )) +
  geom_col(aes(fill= situacao_candidato_turno), position =  "fill")


tabela_chisq<-
as.data.frame(teste_chisq[["stdres"]])


names(tabela_chisq) <- c("partido", "resultado", "residuo_padrao")


grafico<-
tabela_chisq %>%
  filter(resultado == "Eleito") %>%
  mutate(partido = reorder(partido, residuo_padrao),
         sinal = sign(residuo_padrao)) %>%
  ggplot(aes(x= residuo_padrao, y=partido)) +
  geom_col(aes(fill = as.character(sinal)), show.legend = FALSE) +
  geom_vline(aes(xintercept = -2), linetype= "dashed", color = "black") +
  geom_vline(aes(xintercept = 2), linetype= "dashed", color = "black") +
  geom_text(aes(y="REDE", x= 8, label= str_wrap("Valores > 2 são diferenças positivas significativas",20))) +
  geom_text(aes(y="UNIÃO", x= -8, label= str_wrap("Valores < -2 são diferenças negativas significativas",20))) +
  geom_text(aes(y="PC do B", x= 8, label= str_wrap("Cinco partidos desempenharam melhor do que o estatisticamente esperado",20))) +
  geom_text(aes(y="MOBILIZA", x= -10, label= str_wrap("PT: pior desempenho comparando o estatisticamente esperado com o realizado",20))) +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank()
  ) +
  annotate("segment", x=2, y="REDE", yend = "REDE", xend  = 4.5,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
  annotate("segment", x=-2, y="UNIÃO", yend = "UNIÃO", xend  = -4.5,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
  annotate("segment", x=-11, y="REDE", yend = "PSOL", xend  = -12,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1)+
  labs(title = "Prefeitos eleitos: diferença estatística entre o número esperado e o observado",
       subtitle = "Ranking das discrepâncias",
       x="Diferença estatística (resíduo padrão de teste chi-quadrado)",
       y="",
       caption = "Fonte: TSE. Elaboração própria")


ggsave("teste_chi_quadrado.png", plot = grafico, width = 10, height = 5, dpi = 300, type = "cairo-png") 



candidatos_prefeitos_resultado_ne<-
  resultado_eleicao_2024 %>%
  filter(cargo == "Prefeito",
         regiao == "NE") %>%
  select(sg_partido, situacao_candidato_turno)

teste_chisq_ne<-
  chisq.test(candidatos_prefeitos_resultado_ne$sg_partido, candidatos_prefeitos_resultado_ne$situacao_candidato_turno,simulate.p.value = TRUE )




tabela_chisq_ne<-
  as.data.frame(teste_chisq_ne[["stdres"]])


names(tabela_chisq_ne) <- c("partido", "resultado", "residuo_padrao")


grafico<-
  tabela_chisq_ne %>%
  filter(resultado == "Eleito") %>%
  mutate(partido = reorder(partido, residuo_padrao),
         sinal = sign(residuo_padrao)) %>%
  ggplot(aes(x= residuo_padrao, y=partido)) +
  geom_col(aes(fill = as.character(sinal)), show.legend = FALSE) +
  geom_vline(aes(xintercept = -2), linetype= "dashed", color = "black") +
  geom_vline(aes(xintercept = 2), linetype= "dashed", color = "black") +
  geom_text(aes(y="REDE", x= 4.5, label= str_wrap("Valores > 2 são diferenças positivas significativas",20))) +
  geom_text(aes(y="UNIÃO", x= -5, label= str_wrap("Valores < -2 são diferenças negativas significativas",20))) +
  geom_text(aes(y="PSDB", x= 4.3, label= str_wrap("PSB teve o segundo melhor desempenho",20))) +
  scale_fill_discrete_qualitative(palette = "Dark 2") +
  theme_light() +
  theme(
    panel.grid = element_blank()
  ) +
  annotate("segment", x=2, y="REDE", yend = "REDE", xend  = 3,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
  annotate("segment", x=-2, y="UNIÃO", yend = "UNIÃO", xend  = -3,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1) +
  annotate("curve", x=5, y="PC do B", yend = "PSB", xend  = 5.2,  arrow = arrow(length = unit(0.2, "cm")), linewidth = 1)+
  labs(title = "Prefeitos eleitos: diferença estatística entre o número esperado e o observado",
       subtitle = "Ranking das discrepâncias no Nordeste",
       x="Diferença estatística (resíduo padrão de teste chi-quadrado)",
       y="",
       caption = "Fonte: TSE. Elaboração própria")


ggsave("teste_chi_quadrado_ne.png", plot = grafico, width = 10, height = 5, dpi = 300, type = "cairo-png") 


