library(tidyverse)


#link base para downloads

#https://web3.antaq.gov.br/ea/txt/2024.zip

#links para tabelas de apoio
#https://web3.antaq.gov.br/ea/txt/InstalacaoOrigem.zip
#https://web3.antaq.gov.br/ea/txt/InstalacaoDestino.zip
#https://web3.antaq.gov.br/ea/txt/Mercadoria.zip
#https://web3.antaq.gov.br/ea/txt/MercadoriaConteinerizada.zip




# Defina o diretório onde os arquivos estão localizados
diretorio <- "2024/"

# Lista todos os arquivos .txt no diretório
arquivos_txt <- list.files(diretorio, pattern = "\\.txt$", full.names = TRUE)

# Loop sobre cada arquivo .txt
for (arquivo in arquivos_txt) {
  # Extrai o nome do arquivo sem extensão
  nome_sem_extensao <- tools::file_path_sans_ext(basename(arquivo))
  
  # Renomeia o arquivo com a extensão .csv
  novo_nome <- paste0(diretorio, "/", nome_sem_extensao, ".csv")
  file.rename(arquivo, novo_nome)
}

# Verifica se os arquivos foram renomeados corretamente
arquivos_csv <- list.files(diretorio, pattern = "\\.csv$", full.names = TRUE)
print(arquivos_csv)



X2024Atracacao <- read_delim("2024/2024Atracacao.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

X2024Atracacao <- janitor::clean_names(X2024Atracacao)


X2024Carga <- read_delim("2024/2024Carga.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

X2024Carga <- janitor::clean_names(X2024Carga)


X2024Carga_Conteinerizada <- read_delim("2024/2024Carga_Conteinerizada.csv", 
                                        delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                            grouping_mark = "."), trim_ws = TRUE)

X2024Carga_Conteinerizada <- janitor::clean_names(X2024Carga_Conteinerizada)
