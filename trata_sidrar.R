library(sidrar)
library(tidyverse)

get_sidra(x = 1419,
          variable = 63,
          period = c("last" = 12),
          geo = "City",
          geo.filter = 5002407,
          classific = "c315",
          category = list(7169),
          header = FALSE,
          format = 3)



gera_trimestres<- function(anos){
  purrr::map_chr(anos, function(ano){
    print(ano)
    str_trimestres<-
    purrr::map_chr(1:3, function(trimestre){
      print(trimestre)
      texto<-str_c(ano,"0",trimestre)
      print(texto)
      texto
    })
    print(str_trimestres)
    str_trimestres
  })
}


gera_trimestres<- function(anos){
  purrr::map_chr(anos, function(ano){
    print(ano)
    str_trimestres<-
      purrr::map_chr(1:3, function(trimestre){
        print(trimestre)
        texto<-str_c(ano,"0",trimestre)
        print(texto)
        texto
      })
    print(str_trimestres)
    str_trimestres
  })
}


gera_trimestres(2022:2023)

cnt_vt<- 
  get_sidra(x = 6612,
            period = as.character(c("199601":"202303")))

cnt_vt <- janitor::clean_names(cnt_vt)

cnt_vt %>%
  filter(setores_e_subsetores == "Comércio") %>%
  ggplot(aes(x= trimestre_codigo, y= valor)) +
  geom_col()


simula_energia_eletrica<-
rnorm(1000, mean=23000, sd=1000)

mean(simula_energia_eletrica)

energia_ordenado<-
  sort(simula_energia_eletrica)

(energia_ordenado[500] + energia_ordenado[501])/2
  
median(simula_energia_eletrica)

energia_ordenado[250]


energia_ordenado[750]


boxplot(simula_energia_eletrica)
