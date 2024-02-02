library(readr)

X202301_Licitação <- read_delim("202301_Licitacoes/202301_Licitação.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(`Data Resultado Compra` = col_date(format = "%d/%m/%Y"), 
                                                                                     `Data Abertura` = col_date(format = "%d/%m/%Y")), 
                                locale = locale(date_names = "pt", date_format = "d%m%Y", 
                                                decimal_mark = ",", grouping_mark = ".", 
                                                encoding = "Latin1"), trim_ws = TRUE)
