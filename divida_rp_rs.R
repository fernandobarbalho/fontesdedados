library(tidyverse)
library()

info_consolidadas_dividas <- read_delim("20240513-11-info-consolidadas-dividas.csv", 
                                                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                         grouping_mark = ".", encoding = "latin1"), 
                                                     trim_ws = TRUE)

dividas_rs_municipios_estados<-
  info_consolidadas_dividas %>%
  filter(UF=="RS")


dividas_rs_municipios_estados <- janitor::clean_names(dividas_rs_municipios_estados)


dividas_rs_municipios_estados %>%
  writexl::write_xlsx("dividas_rs_municipios_estados.xlsx")


