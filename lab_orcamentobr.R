
library(orcamentoBR)
despesaDetalhada(2020, UO = "73901", valorPLOA = FALSE, detalheMaximo = TRUE)


orcamentoBR::quaisMembros(dimensao = "Esfera")

orcamentoBR::quaisMembros(dimensao = "Orgao")

orcamentoBR::quaisMembros(dimensao = "UO")

orcamentoBR::quaisMembros(dimensao = "Funcao")

despesas_mdic<-
  despesaDetalhada(2023, Orgao = "28000", Funcao = TRUE)

despesas_mdic %>%