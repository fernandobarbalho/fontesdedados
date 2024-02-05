library(sidrar)
library(tidyverse)

lubridate::q
get_sidra(x = 1419,
          variable = 63,
          period = c("last" = 12),
          geo = "City",
          geo.filter = 5002407,
          classific = "c315",
          category = list(7169),
          header = FALSE,
          format = 3)




gera_trimestres<- function(ano){
  
  ano<-ano
  purrr::map_chr(1:3, function(trimestre){
    stringr::str_c(ano,"0", trimestre)
  })
}


gera_meses_trimestre<- function(trimestre_codigo){
  as.Date(paste0(
    str_sub(trimestre_codigo,1,4),
    "-",
    (as.numeric(str_sub(trimestre_codigo,5,6)))*3,
    "-01"))
}


gera_meses_trimestre("201603")

calculate_next_month <- function(input_date) {
  input_date <- as.Date(input_date)  # Convert input to Date object
  next_month <- input_date %m+% months(1)  # Add 1 month to the input date
  return(next_month)
}


lista_trimestres<- unlist(lapply(1996:2023, gera_trimestres)) 




cnt_vt<- 
  get_sidra(x = 6612,
            period = lista_trimestres)

cnt_vt <- janitor::clean_names(cnt_vt)


cnt_vt %>%
  filter(setores_e_subsetores_codigo %in% c("90687", "90691", "90696")) %>%
  ggplot(aes(x = trimestre_codigo, y = valor, group = setores_e_subsetores, color = setores_e_subsetores)) +
  geom_line() 


lubridate::month("2016-03-01")



cnt_vt_precos_correntes<- 
  get_sidra(x = 1846,
            period = lista_trimestres)

cnt_vt_precos_correntes <- janitor::clean_names(cnt_vt_precos_correntes)


cnt_vt_precos_correntes %>%
  filter(setores_e_subsetores_codigo %in% c("90687", "90691", "90696")) %>%
  ggplot(aes(x = trimestre_codigo, y = valor, group = setores_e_subsetores, color = setores_e_subsetores)) +
  geom_line() 



cnt_vt_precos_correntes %>%
  filter(setores_e_subsetores_codigo %in% c("90687", "90691", "90696")) %>%
  mutate(data_nominal = gera_meses_trimestre(trimestre_codigo)) %>%
  mutate(data_calculo = calculate_next_month(data_nominal)) %>%
  mutate(valor_constante = deflate(valor,  data_calculo,"09/2023", index= "ipca") )  %>%
  ggplot(aes(x = data_nominal, y = valor_constante, group = setores_e_subsetores, color = setores_e_subsetores)) +
  geom_line() 
  
  

fab<-
  cnt_vt_precos_correntes %>%
  filter(setores_e_subsetores_codigo %in% c("90687", "90691", "90696")) %>%
  mutate(data_nominal = gera_meses_trimestre(trimestre_codigo)) %>%
  mutate(data_calculo = calculate_next_month(data_nominal)) %>%
  mutate(valor_constante = deflate(valor,  data_calculo,"09/2023", index= "ipca") )


ipca_num_indice<- 
  get_sidra(x=1737,
            variable = 2266)
  


# Load the package
library(deflateBR)

# Deflate January 2000 reais
deflate(nominal_values = 100, nominal_dates = as.Date("2000-01-01"), real_date = "01/2018")
#> 
#> Downloading necessary data from IPEA's API
#> ...
#> [1] 310.3893
#> 
#> 



deflate(nominal_values = 179803.73517934, nominal_dates = as.Date("2023-12-30"), real_date = "12/2023", index = "ipca")


inflation("11/2023", "12/2023", "ipca")


179803.73517934 * (1+inflation("12/2023", "12/2023", "ipca")/100)


inflation("12/2023", "12/2023", "ipca")

6773.2700 / 6735.5500


library(lubridate)

calculate_next_month <- function(input_date) {
  input_date <- as.Date(input_date)  # Convert input to Date object
  next_month <- input_date %m+% months(1)  # Add 1 month to the input date
  return(next_month)
}

# Example usage:
input_date <- "2024-02-05"  # Replace with your desired input date
next_month_date <- calculate_next_month(input_date)
print(next_month_date)
