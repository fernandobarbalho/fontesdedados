library(tesseract)
library(stringr)
library(tidyverse)


text<- ocr("ma11.png")


ocr_text<- "AC 08 6.834 547 1.603 968 1.255 128 440 11.775\nAL 3,1 13.130 2.293 8.981 3.989 4.544 291 420 33.649\nAM 3,9 11.776 2.037 6.185 4.035 5.498 678 775 30.985\nAP 07 6.636 564 991 896 1.037 137 382 10.643\nBA 141 40.691 7.569 44.376 17.037 18.247 1.185 3.124 132.228\nCE 88 26.545 6.196 25.648 10.201 11.705 1.215 1.733 83.243\nDF 28 18.716 1.849 8.234 1.867 11.729 21.647 2.138 66.179\nES 3,8 10.102 2.068 13.548 2.733 5.725 616 856 (35.647\nGO 71 13.749 3.466 16.857 5.100 9.616 1.776 1.687 52.252\nMA 68 26.463 3.845 16.962 7.327 8.419 470 1.050 64.536\nMG 20,5 38.926 11.095 80.620 15.770 31.160 3.016 6.083 186.671\nMS 28 6.027 1.523 7.231 2.344 4.180 538 596 22.440\nMT 3,7 9.320 1.905 7.693 2.506 5.199 430 521 27.576\nPA 81 26.559 3.488 15.149 8.482 10.968 902 921 66.469\nPB 4,0 14.864 2.510 12.304 4.656 7.089 528 709 42.660\nPE 91 22.334 5.227 27.229 11.364 13.468 3.094 1.238 83.954\nPI 3,3 13.717 2.336 10.241 3.736 4.613 323 678 35.644\nPR 114 20.827 5.877 42.693 6.889 15.729 1.951 2.512 96.477\nRJ 16,1 61.113 7.592 70.410 12.700 29.270 8.607 2.670 192.361\nRN 3,3 12.158 1.935 9.764 3.401 5.809 535 807 34.410\nRO 16 6.438 1.000 4.136 1.269 2.378 256 545 16.023\nRR 0,6 5.519 380 746 506 977 187 531 ‘8.847\nRS. 10,9 18.209 5.357 57.643 6.604 20.882 2.331 1.892 112.916\nSC 7,6 10.990 3.808 33.265 2.973 11.164 1.790 1.106 65.096\nSE 2,2 9.627 1.225 6.488 2.549 3.305 209 361 23.764\nSP 444 44.278 19.415 201.974 25.851 54.204 10.741 6.349 362.811\nTO 15 9.388 1.137 3.378 1.320 2.061 179 455 17.918\nTotal 203 504.939 106.244 734.350 167.073 300.230 63.760 40.577 1.917.173\n"

ocr_text<- stringr::str_remove_all(ocr_text,"[.]")

# Divida o texto em linhas
lines <- strsplit(ocr_text, "\n")[[1]]

# Cada linha é dividida em colunas (aqui assumimos que o espaço é o separador)
data <- lapply(lines, function(line) strsplit(line, " "))

# Converta em um data frame (você precisará ajustar isso com base em quantas colunas você tem)
df <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T))

df_trabalho<- df[,-2]

df_trabalho[8,9]<-"35647"

# Função para converter colunas para numérico, exceto a primeira
convert_to_numeric_except_first <- function(df) {
  df <- df %>%
    mutate_at(vars(-1), as.numeric) # -1 indica todas as colunas exceto a primeira
  return(df)
}

# Exemplo de uso
df_trabalho <- df_trabalho %>%
  convert_to_numeric_except_first()

df_trabalho[22,9]<- 8847

names(df_trabalho)<- c("uf", "transferencias_para_em","fundos_saude_educacao", "beneficios_inss","outros_beneficios_sociais","gastos_pessoal","custeio","obras_equipamentos","total")


df_trabalho%>%
  writexl::write_xlsx("gastos_uniao_uf.xlsx")
  


text<- ocr("ma2.png")


ocr_text<- "AC 08 1.717,1 5818 53,3 399,0 18 1.110,5 1.030,3 4.894\nAL 3,1 5.662,5 1.702,9 414.6 2.292,9 6,1 4.184,4 3.286,7 17.550\nAM 3,9 6.901,4 2.2246 459,1 4.245,9 3,1 5.273,0 5.668,6 24.776\nAP 07 1.493,3 646,5 67,0 354,4 1,0 981,4 641,0 4.185\nBA 14,1 28.722,9 6.961,2 2.775,0 14.380,8 83,4 18.913,5 18.715,7 90.552\nCE 88 16.030,6 4.619,2 1.2564 8.224,4 6,3 11.762,6 12.308,0 54.207\nDF 28 12.890,7 9.170,6 2.289,9 10.776,8 3,7 3.769,0 23.929,3 62.830\nES 3,8 9.954,2 3.339,1 1.741,5 7.471,4 15,0 5.1289 8.787,9 36.433,\nGO 71 17.255,1 5.820,1 1.975,4 13.574,3 329,0 9.439,4 12.190,9 60.584\nMA 68 8.932,5 2.286,7 441,6 3.608,0 23,5 9.064,6 5.229,4 29.586\nMG 20,5 54.243,6 18.261,2 8.864,2 38.7111 250,8 27.479,2 47.453,0 195.263\nMS 28 6.835,1 3.087,7 1.140,8 5.297,9 401,1 3.688,3 5.370,9 25.822\nMT 3,7 7.686,0 3.7148 1.3118 9.364,4 380,4 4.895,2 8.640,5 35.993\nPA 81 15.676,1 4.015,1 742,3 4.925,8 449 10.858,8 8.5440 44.807\nPB 4,0 7.026,6 2.216,5 401,6 2.819,6 4,0 5.317,6 4.608,6 22.395\nPE 91 16.927,2 5.510,1 1.752,7 10.133,6 9,9 12.114,9 11.430,8 57.879\nPI 3,3 5.912,1 1.535,0 371,0 2.516,0 26,7 4.373,9 3.202,5 17.937\nPR 114 36.389,0 11.975,0 7.736,3 31.709,7 319,3 15.310,1 31.725,8 135.165\nRJ 16,1 53.505,5, 25.538,1 12.239,9 36.203,0 15,1 21.479,7 49.256,4 198.238\nRN 3,3 7.114,3 2.3766 384,38 3.066,6 3,1 4.4184 3.942,8 21.307\nRO 16 3.993,3 1.202,0 257,0 1.644,9 20,1 2.115,3 2.378,3 11.611\nRR 0,6 1.167,2 574,1 62,7 444,1 3,1 851,3 779,1 3.882\nRS. 10,9 40.229,3 13.1915 7.273,5 29.756,7 222,2 14.557,3 32.091,7 137.322\nSc 7,6 26.665,6 8.309,3 5.049,1 25.236,2 50,8 10.181,1 27.930,4 103.422\nSE 2,2 4.332,7 15918 311,2 1.736,5 3,5 2.956,2 2.782,7 13.715\nSP 444 160.979,7 69.858,0 52.386,3 174.398,3 472,8 59.431,2 201.540,6 719.067\nTO 15 3.279,5 1.040,7 213,1 1.5348 57,2 2.022,2 2.244,5 10.392\nTotal 203 561.523 211.350 111.972 444.827 2.758, 271.678 535.710 2.139.818\n"

ocr_text<- stringr::str_remove_all(ocr_text,"[.]")


ocr_text<- stringr::str_remove_all(ocr_text,"[,]")


# Divida o texto em linhas
lines <- strsplit(ocr_text, "\n")[[1]]

# Cada linha é dividida em colunas (aqui assumimos que o espaço é o separador)
data <- lapply(lines, function(line) strsplit(line, " "))

# Converta em um data frame (você precisará ajustar isso com base em quantas colunas você tem)
df <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T))

df_trabalho_receita<- df[,-2]

df_trabalho_receita[24,1]<-"SC"

df_trabalho_receita <- df_trabalho_receita %>%
  convert_to_numeric_except_first()

names(df_trabalho_receita)<- c("uf", "tributos_bens_servicos","irpf_irrf_trabalho", "irff_capital","irpj_csll", "itr", "royalties_dividendo_irrf_menos_exportacao","receita_previdenciaria","total")


df_trabalho_receita<-
df_trabalho_receita %>%
  pivot_longer(cols = -1,
               names_to = "fonte",
               values_to = "valor") %>%
  mutate(valor  = valor/10) %>%
  pivot_wider(names_from = fonte,
              values_from = valor) %>%
  filter(uf!="Total")

df_trabalho_receita[20,4]<- 384.8

df_trabalho_receita$total <- df_trabalho_receita$total*10


df_trabalho_receita%>%
  writexl::write_xlsx("receitas_uniao_uf.xlsx")

df_trabalho %>%
  filter(uf!="Total") %>%
  pivot_longer(cols = -1,
               names_to = "rubrica",
               values_to = "valor") %>%
  filter(rubrica=="total")
  

