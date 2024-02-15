library(readr)
GFSR_taxes_social_contributuion <- read_csv("GFSR_02-15-2024 12-40-17-36_timeSeries/GFSR_02-15-2024 12-40-17-36_timeSeries.csv")

GFSR_taxes_social_contributuion <- janitor::clean_names(GFSR_taxes_social_contributuion)


df_trabalho<-
  GFSR_taxes_social_contributuion %>%
  filter(classification_code %in% c("W0|S1|G12","W0|S1|G11"),
         attribute == "Value") %>%
  select(c(1,3,10:22)) %>%
  pivot_longer(cols = -(1:2), names_to = "ano", values_to = "valor") %>%
  mutate(ano = str_sub(ano,2,5),
         valor =as.numeric(valor))

df_trabalho %>%
  filter(ano == 2021)%>%
  summarise(carga_total =  sum(valor),
            .by = c(country_name, classification_name)) %>%
  mutate(country_name = case_when(
    country_name == "Brazil" ~"Brasil",
    country_name =="Russian Federation" ~"Rússia",
    country_name =="South Africa" ~"África do Sul",
    country_name =="China, P.R.: Mainland" ~"China"
  )) %>%
  mutate(classification_name = ifelse(classification_name =="Tax revenue","Impostos","Contribuições Sociais")) %>%
  mutate(country_name = reorder(country_name, carga_total)) %>%
  ggplot(aes(x=carga_total, y=country_name)) +
  geom_col(aes(fill= classification_name)) +
  theme_light() +
  labs(
    title = "Carga tributária BRICS sem Índia",
    subtitle = "ano: 2021",
    caption =  "Fonte: FMI",
    x="",
    y="",
    fill= "tipo de tributo"
  )

