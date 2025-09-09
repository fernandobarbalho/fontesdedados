library(tidyverse)
library(orcamentoBR)

despesaDetalhada(2023, UO = "73901", valorPLOA = FALSE)


orcamentoBR::quaisMembros(dimensao = "Esfera")

orcamentoBR::quaisMembros(dimensao = "Orgao")

orcamentoBR::quaisMembros(dimensao = "UO")

orcamentoBR::quaisMembros(dimensao = "Funcao")

fontes<-
orcamentoBR::quaisMembros(dimensao = "Fonte")

gnd<-
orcamentoBR::quaisMembros(dimensao = "Fonte")

rp<-
  orcamentoBR::quaisMembros(dimensao = "Fonte")
  

despesas_mdic<-
  despesaDetalhada(2023)

despesas_mdic_funcao<-
  despesaDetalhada(2023, Orgao = "28000", Funcao = TRUE, timeout = 1606)


despesas_mec_funcao<-
  despesaDetalhada(2023, Orgao = "26000", Funcao = TRUE)


despesas_mec<-
  despesaDetalhada(2023, Orgao = "26000")


despesas_funcao_industria<-
  despesaDetalhada(2023, Orgao = TRUE,  Funcao = "04")


despesas_funcao_industria

  despesaDetalhada(2023,  Funcao = 2)


despesas_mdic %>%
  mutate(Funcao_desc = reorder(Funcao_desc, pago)) %>%
  ggplot(aes(x=pago, y= Funcao_desc)) +
  geom_col()

despesas_funcao<-
  despesaDetalhada(exercicio = 2023, Funcao = TRUE)

despesas_modalidade_aplicacao<-
  despesaDetalhada(exercicio = 2023, ModalidadeAplicacao  = TRUE)


despesa_orgao_funcao<-
  despesaDetalhada(exercicio = 2023, Funcao = TRUE, Esfera  = TRUE)


despesa_rp_funcao<-
  despesaDetalhada(exercicio = 2023, Funcao = TRUE, ResultadoPrimario    = TRUE)

despesa_orgao<-
  despesaDetalhada(exercicio = 2023, Orgao = TRUE)


despesaDetalhada(exercicio = 2023, ResultadoPrimario = "6")
  

despesas_funcao %>%
  mutate(Funcao_desc = reorder(Funcao_desc, pago)) %>%
  ggplot(aes(x=pago, y= Funcao_desc)) +
  geom_col()
