internacoes_rs_principais_cidades <- read_csv("Deslocamentos e gastos hospitalares dos municípios.csv", 
                                              locale = locale(decimal_mark = ",", grouping_mark = "."))


municipios_calamidade_ibge <- readRDS("~/github/fontesdedados/municipios_calamidade_ibge.rds")


dados_mapa_e_cidade <- read_csv("dados_mapa_e_cidade.csv")


internacoes_rs_principais_cidades <- janitor::clean_names(internacoes_rs_principais_cidades)


internacoes_trabalho<-
  internacoes_rs_principais_cidades %>%
  mutate(codigo_municipio = as.character(codigo_municipio)) %>%
  left_join(
    municipios_calamidade_ibge %>%
      mutate(codigo_municipio = as.character(id_municipio),
             codigo_municipio = str_sub(codigo_municipio, 1,6))
  )



internacoes_trabalho<-
internacoes_trabalho %>%
  mutate_at(vars(entrada_de_paciente:saida_de_paciente), funs(. * 10))


saveRDS(internacoes_trabalho,"internacoes_trabalho.rds" )

internacoes_trabalho %>%
  mutate(tipo_registro = ifelse(is.na(tipo_registro),"Não registrado", tipo_registro)) %>%
  summarise(total_entrada = sum(entrada_de_paciente),
            total_atendimento_local = sum(atendimento_local),
            total_atendimento = total_entrada+total_atendimento_local,
            .by = tipo_registro)



internacoes_trabalho %>%
  mutate(tipo_registro = ifelse(is.na(tipo_registro),"Não registrado", tipo_registro)) %>%
  ggplot(aes(x= tipo_registro, y=percent_gasto)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = tipo_registro))
  