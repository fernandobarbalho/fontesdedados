library(tidyverse)

recorte_reuniao <- readRDS("~/github/fontesdedados/recorte_reuniao.rds")

names(recorte_reuniao)

# install.packages("remotes")
#remotes::install_github("rfsaldanha/microdatasus")


library(microdatasus)

# Vetor com as siglas dos UFs brasileiros
ufs <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", 
         "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", 
         "RR", "SC", "SP", "SE", "TO")


# Dividir as siglas dos UFs em três grupos
grupo1 <- ufs[1:9]
grupo2 <- ufs[10:18]
grupo3 <- ufs[19:27]

dados <- fetch_datasus(year_start = 2022, year_end = 2022, uf = "AC", information_system = "SIH-RD")
dados <- process_sim(dados)

dados_teste<-
microdatasus::fetch_datasus(year_start = 2020,
                            year_end = 2020,
                            uf = "AC",
                            month_start = 1,
                            month_end = 12,
                            information_system = "SIH-RD")


sih_grupo_1<-
  purrr::map2_dfr(grupo1, function(a_uf){
    dados <- fetch_datasus(year_start = 2022, year_end = 2022, uf = "RJ", information_system = "SIH")
    dados <- process_sim(dados)
  })



internacoes_neoplasia<- recorte_reuniao

internacoes_neoplasia<- janitor::clean_names(internacoes_neoplasia)

definir_faixa_etaria <- function(idade) {
  faixa <- character(length(idade))  # Cria um vetor vazio para armazenar as faixas etárias
  
  faixa[idade >= 0 & idade <= 14] <- "0-14 anos"
  faixa[idade >= 15 & idade <= 44] <- "15-44 anos"
  faixa[idade >= 45 & idade <= 64] <- "45-64 anos"
  faixa[idade >= 65] <- "65 anos ou mais"
  
  return(faixa)
}

# Acrescentar a coluna faixa_etaria à tabela internacoes_neoplasia
internacoes_neoplasia <- internacoes_neoplasia %>%
  mutate(faixa_etaria = definir_faixa_etaria(idade))

internacoes_neoplasia <-
  internacoes_neoplasia %>%
  mutate(val_tot = as.numeric(val_tot) )

internacoes_neoplasia <-
  internacoes_neoplasia %>%
  mutate(idade = as.numeric(idade) )



mean(internacoes_neoplasia$idade, na.rm = TRUE)
median(internacoes_neoplasia$idade, na.rm = TRUE)

mean(internacoes_neoplasia$val_tot)
median(internacoes_neoplasia$val_tot)

modelo<- lm(log(val_tot)~faixa_etaria+sexo+morte, data = internacoes_neoplasia[internacoes_neoplasia$val_tot>0,])

summary(modelo)


internacoes_neoplasia %>%
  summarise(.by=faixa_etaria,
            media_valor_total = mean(val_tot, na.rm = TRUE))

internacoes_neoplasia %>%
  ggplot(aes(x= faixa_etaria, y= val_tot))+
  geom_boxplot() +
  scale_y_log10()



internacoes_neoplasia %>%
  summarise( percentual = (n()/NROW(internacoes_neoplasia)*100),
             .by= c(morte))


internacoes_neoplasia %>%
  summarise( percentual = (n()/NROW(internacoes_neoplasia)*100),
             .by= c(morte, sexo))



internacoes_neoplasia %>%
  summarise( percentual = (n()/NROW(internacoes_neoplasia)*100),
             .by= c(morte, faixa_etaria))



internacoes_neoplasia %>%
  summarise( percentual = (n()/NROW(internacoes_neoplasia)*100),
             .by= c(sexo, faixa_etaria))


internacoes_neoplasia %>%
  summarise( percentual = (n()/NROW(internacoes_neoplasia)*100),
             .by= c(sexo, faixa_etaria, morte))



