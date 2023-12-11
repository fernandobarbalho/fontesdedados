
#install.packages("pdftools")

if (!require("remotes")) {
  install.packages("remotes")
}
# on 64-bit Windows
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# elsewhere
#remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"))

library(tabulizer)
library(tidyverse)
library(pdftools)

# Sample DataFrame
# Replace this with your actual DataFrame
df <- data.frame(col1 = c("text", "more.text", "text&"),
                 col2 = c("123", "4,56", "789"),
                 col3 = c("hello", "world!", "R"))

# Function to check for special characters
has_special_chars <- function(x) {
  any(grepl("[^[:alnum:] ,.]", x))
}

# Apply the function to each column and get column names with special characters
columns_with_special_chars <- names(df)[sapply(df, has_special_chars)]

# Print the result
print(columns_with_special_chars)



extrai_tabelas_anuais <- function(pdf_path, ano, mes, a_tabela) {
  
  tabelas_prisma_fiscal <- readr::read_csv("tabelas_prisma_fiscal.csv")
  
  coords_tabela<-
    tabelas_prisma_fiscal%>%
    filter(tabela==a_tabela)
  
  
  # Extract tables from the PDF
  table <- tabulizer::extract_tables(pdf_path, 
                                      pages = coords_tabela$pagina,
                                      guess = FALSE, 
                                      method = "lattice", 
                                      area = list(c(coords_tabela$top * 72, 0, coords_tabela$bottom * 72, 10 * 72)))
  
  
  table<- as.data.frame (table)
  
  columns_with_special_chars <- names(table)[sapply(table, has_special_chars)]
  
  if (NROW(columns_with_special_chars)>0){
    table<- 
      table %>%
      select(-all_of(columns_with_special_chars))
      
  }
  
  names(table)<- c("medida","mes_atual_ano_atual","mes_anterior_ano_atual","mes_atual_ano_futuro","mes_anterior_ano_futuro")
  # Convert each table to a DataFrame
  
  table$estatistica<- a_tabela
  table$ano<- ano
  table$mes <-mes
  
  table<-
  table%>%
    mutate(across(2:5,~str_replace_all(.x,"[.]",""))) %>%
    mutate(across(2:5,~str_replace_all(.x,",","."))) %>%
    mutate(across(2:5,as.numeric))
  
  table%>%
    select(c(6:8,1,2:5))
}




# Teste com relatório de novembro de 2023
pdf_path <- "Relatorio Mensal 2023_11-v2.pdf"



# Extract tables

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                           ano =2023,
                                           mes = 11,
                                           "Arrecadação das Receitas Federais")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 11,
                                         "Receita Líquida do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 11,
                                         "Despesa Total do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 11,
                                         "Resultado Primário do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 11,
                                         "Resultado Nominal do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 11,
                                         "Dívida Bruta do Governo Geral")




# Teste com relatório de setembro de 2023
pdf_path <- "relatorio_mensal_setembro_2023-v2.pdf"



# Extract tables

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Arrecadação das Receitas Federais")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Receita Líquida do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Despesa Total do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Resultado Primário do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Resultado Nominal do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Dívida Bruta do Governo Geral")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "Deflator")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "PIB Nominal")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 09,
                                         "INPC")



# Teste com relatório de outubro de 2023
pdf_path <- "relatorio_mensal_outubro_2023_v2.pdf"



# Extract tables

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Arrecadação das Receitas Federais")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Receita Líquida do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Despesa Total do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Resultado Primário do Governo Central")


extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Resultado Nominal do Governo Central")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Dívida Bruta do Governo Geral")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "Deflator")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "PIB Nominal")

extracted_table <- extrai_tabelas_anuais(pdf_path,
                                         ano =2023,
                                         mes = 10,
                                         "INPC")


#laboratório
pdf_text <- pdf_text(pdf_path)


