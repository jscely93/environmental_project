####################
# Realizador: Juan Sebastian Cely Acosta
# Fecha: 30/01/2023
# Projecto: Percepción y conocimiento del cambio climático de jóvenes en Colombia - Prueba técnica de análisis de datos
####################

# Cargar los paquetes requeridos

library(readxl)
library(tidyverse)
library(papaja)
library(flextable)
library(gtsummary)
library(ordinal)
library(MASS)
library(rcompanion)
library(brant)

options(scipen = 999)

##########################

# Importar y explorar la base de datos

raw_data <- read_excel("data_raw_names_corrected.xlsx")
view(raw_data)
glimpse(raw_data)

# Filtrar la base de datos a partir de la pregunta de investigación
# Pregunta de investigación:
# ¿El sector en el que vive una persona y el número de años que lleva viviendo en él
# influencian la opinión que esta tiene sobre el uso de bosques y humedales?
# Hipótesis:
# Hipótesis 1: Entre más años lleva viviendo una persona en una zona rural,
# es más probable que esté en desacuerdo con usar como lugares públicos bosques y humedales.
# Hipótesis 2: Entre más años lleva viviendo una persona en una zona urbana,
# es más probable que esté de acuerdo con usar como lugares públicos bosques y humedales.
# variables relevantes:
# id (identificador), resid_in_dept(Reside en el municipio), years_resid (Años de residencia en el municipio), env_home (Sector de vivienda), for_wet_pub (Deberíamos usar como lugares públicos los bosques y humedales)
# Filtrar por sujetos que se encuentren residiendo en el municipio y selección de variables relevantes

filtered_data <- raw_data %>% filter(resid_in_dept == "TRUE") %>%
  dplyr::select(years_resid, env_home, for_wet_pub)

# Remover sujetos con datos perdidos

filtered_data[filtered_data == "NA"] <- NA
cleaned_data <- na.omit(filtered_data)

# Convertir en factor las variables categoricas

cleaned_data$env_home <- factor(cleaned_data$env_home)
levels(cleaned_data$env_home) <- c(1,2,3)
cleaned_data$env_home <- factor(cleaned_data$env_home, labels = c("Rural", "Suburbano", "Urbano"))

cleaned_data$for_wet_pub <- factor(cleaned_data$for_wet_pub)
levels(cleaned_data$for_wet_pub) <- c(1,2,3,4,5)
cleaned_data$for_wet_pub <- factor(cleaned_data$for_wet_pub, labels = c("Muy en desacuerdo", 
                            "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo"), ordered = TRUE)

###########################

# Análisis exploratorio: Explorar cada una de las variables e identificar outliers

summary(cleaned_data)


tbl_resumen_1 <- cleaned_data %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} [{median}] ({sd})", label = c(years_resid ~ "Años de residencia", env_home ~ "Sector", for_wet_pub ~ "Valoración")) %>%
  modify_header(label ~ "**Característica**") %>%
  modify_footnote(
    all_stat_cols() ~ "M [Mdn] (SD); n (%)"
  )
tbl_resumen_1 %>% as_flex_table() %>%
  save_as_docx(tbl_resumen_1, path = "resumen.docx")
tbl_resumen_1


# Distribución de years_resid (años de residencia en el municipio)


pl1 <- cleaned_data %>% 
  ggplot(aes(x = years_resid)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Años de residencia", y = "Número de casos") +
  papaja::theme_apa()
pl1

cleaned_data %>%
  dplyr::slice_max(n = 5, years_resid)

# Se identificó una persona con 257 años de residencia en el municipio. Como este dato no tiene sentido es removido.

# remover outlier

cleaned_data <- cleaned_data %>%
  filter(years_resid < 100)

pl2 <- cleaned_data %>% 
  ggplot(aes(x = years_resid)) +
  geom_histogram(binwidth = 5) +
  labs(x = "Años de residencia", y = "Número de casos") +
  papaja::theme_apa()
pl2

# Distribución de env_home (Sector de vivienda)

pl3 <- cleaned_data %>% 
  ggplot(aes(x = env_home, fill = env_home)) +
  geom_bar() +
  labs(x = "Sector de vivienda", y = "Número de casos") +
  papaja::theme_apa() +
  theme(legend.position="none")
pl3

# Distribución de for_wet_pub (Deberíamos usar como lugares públicos los bosques y humedales)

pl4 <- cleaned_data %>% 
  ggplot(aes(x = for_wet_pub, fill = for_wet_pub)) +
  geom_bar() +
  labs(x = "Valoración", y = "Número de casos") +
  scale_fill_discrete(name = "Valoración") +
  coord_flip() +
  papaja::theme_apa() +
  theme(legend.position="none")
pl4

#################################

# Explorar visualmente la relación entre variables

# Relación entre opinión sobre uso de áreas naturales y sector de residencia

pl5 <- cleaned_data %>%
  ggplot(aes(x = env_home, fill = for_wet_pub)) +
  geom_bar(position = "fill") +
  labs(x = "Sector", y = "Proporción", fill = "Valoración") +
  papaja::theme_apa()
pl5

tbl_2 <- cleaned_data %>% 
  group_by(env_home, for_wet_pub) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

pl6 <- tbl_2 %>%
  ggplot(aes(x = env_home, y = perc*100, fill = for_wet_pub)) +
  geom_bar(stat="identity") +
  labs(x = "Sector", y = "Porcentaje", fill = "Valoración") +
  papaja::theme_apa()
pl6

# Relación entre opinión sobre uso de áreas naturales, sector de residencia, y años de residencia

pl7 <- cleaned_data %>%
  ggplot(aes(x = years_resid, y = env_home)) +
  geom_boxplot() +
  labs(x = "Años de residencia", y = "Sector", fill) +
  facet_wrap(~for_wet_pub)
pl7

#############################

### Realizar una regresión ordinal para probar las hipótesis

# Comparar el modelo base con el modelo propuesto

modelo_nulo <- clm(for_wet_pub ~ 1,
                   data = cleaned_data,
                   link = "logit")

modelo_1 <- clm(for_wet_pub ~ env_home * years_resid,
                data = cleaned_data,
                link = "logit")


anova(modelo_nulo, modelo_1)

nagelkerke(fit = modelo_1,
           null = modelo_nulo)

summary(modelo_1)

# No hay una diferencia significativa entre los dos modelos,
# sin embargo, el modelo base o nulo presenta un mejor ajuste que el modelo propuesto basandonos en el AIC.


m <- polr(for_wet_pub ~ env_home * years_resid, data = cleaned_data, Hess = TRUE)
summary(m)

ctable <- coef(summary(m))

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

ctable <- cbind(ctable, "p value" = p)

ctable

ci <- confint(m)

exp(coef(m))
exp(cbind(OR = coef(m), ci))

tbl_logreg <- tbl_regression(modelo_1, exponentiate = TRUE,
                             label = c(years_resid ~ "Años de residencia", env_home ~ "Sector")) %>%
                              modify_header(label ~ "**Característica**")
tbl_logreg
