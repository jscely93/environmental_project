###########################
# Realizador: Juan Sebastian Cely Acosta
# Fecha: 30/01/2023
# Projecto: Percepción y conocimiento del cambio climático de jóvenes en Colombia - Prueba técnica de análisis de datos
###########################
#### Cargar los paquetes requeridos

library(readxl)
library(tidyverse)
library(papaja)
library(flextable)
library(gtsummary)
library(ordinal)
library(MASS)
library(rcompanion)
library(brant)

# Mostrar los números decimales completos para facilitar su interpretación
options(scipen = 999)

##########################
#### Importar y revisar la base de datos

raw_data <- read_excel("data_raw_names_corrected.xlsx")
view(raw_data)
glimpse(raw_data)

# Filtrar la base de datos a partir de la pregunta de investigación

# Pregunta de investigación:

# ¿El sector en el que vive una persona y el número de años que lleva viviendo en él
# influencian la opinión que esta tiene sobre el uso de bosques y humedales?

# Variables relevantes:
# resid_in_dept(Reside en el municipio), years_resid (Años de residencia en el municipio),
# env_home (Sector de vivienda), for_wet_pub (Deberíamos usar como lugares públicos los bosques y humedales)

# Identificar el número de datos perdidos en la base de datos y en las variables de interés

sum(is.na(raw_data))
sum(is.na(raw_data$resid_in_dept))
sum(is.na(raw_data$years_resid))
sum(is.na(raw_data$env_home))
sum(is.na(raw_data$for_wet_pub))

# Filtrar por sujetos que se encuentren residiendo en el municipio y selección de las otras variables relevantes
filtered_data <- raw_data %>% filter(resid_in_dept == "TRUE") %>%
  dplyr::select(years_resid, env_home, for_wet_pub)

# Remover sujetos con datos perdidos

filtered_data[filtered_data == "NA"] <- NA
cleaned_data <- na.omit(filtered_data)

# Convertir en factor las variables categoricas y poner nombres adecuados a los niveles de cada una

cleaned_data$env_home <- factor(cleaned_data$env_home)
levels(cleaned_data$env_home) <- c(1,2,3)
cleaned_data$env_home <- factor(cleaned_data$env_home, labels = c("Rural", "Suburbano", "Urbano"))

cleaned_data$for_wet_pub <- factor(cleaned_data$for_wet_pub)
levels(cleaned_data$for_wet_pub) <- c(1,2,3,4,5)
cleaned_data$for_wet_pub <- factor(cleaned_data$for_wet_pub, labels = c("Muy en desacuerdo", 
                            "En desacuerdo", "Neutral", "De acuerdo", "Muy de acuerdo"), ordered = TRUE)

###########################
#### Análisis exploratorio: Explorar cada una de las variables e identificar outliers

# Estadísticos descriptivos
summary(cleaned_data)

# Distribución de years_resid (años de residencia en el municipio)

pl1 <- cleaned_data %>% 
  ggplot(aes(x = years_resid)) +
  geom_histogram(binwidth = 2) +
  geom_vline(aes(xintercept = mean(years_resid), colour = "red")) +
  labs(x = "Años de residencia", y = "Número de casos") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  ggtitle("Distribución \n Años de residencia con outlier") +
  papaja::theme_apa() +
  theme(legend.position="none", text = element_text(size = 12))
pl1

# Los 5 datos más altos de la variable Años de residencia
cleaned_data %>%
  dplyr::slice_max(n = 5, years_resid)

# Se identificó una persona con 257 años de residencia en el municipio.
# Como este dato no tiene sentido es removido.

# Remover outlier
cleaned_data <- cleaned_data %>%
  filter(years_resid < 100)

# Observar distribución sin outlier y con un ancho de barras que facilite
# su interpretación

pl2 <- cleaned_data %>% 
  ggplot(aes(x = years_resid)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept = mean(years_resid), colour = "red")) +
  labs(x = "Años de residencia", y = "Número de casos") +
  ggtitle("Distribución \n Años de residencia") +
  papaja::theme_apa() +
  theme(legend.position="none")
pl2

# Distribución de env_home (Sector de vivienda)

pl3 <- cleaned_data %>% 
  ggplot(aes(x = env_home, fill = env_home)) +
  geom_bar() +
  labs(x = "Sector de vivienda", y = "Número de casos") +
  ggtitle("Distribución \n Sector de vivienda") +
  papaja::theme_apa() +
  theme(legend.position="none")
pl3

# Distribución de for_wet_pub (Deberíamos usar como lugares públicos los bosques y humedales)

pl4 <- cleaned_data %>% 
  ggplot(aes(x = for_wet_pub, fill = for_wet_pub)) +
  geom_bar() +
  labs(x = "Valoración", y = "Número de casos") +
  ggtitle("Distribución \n ¿Deberíamos usar como lugares públicos \n los bosques y humedales?") +
  scale_fill_discrete(name = "Valoración") +
  coord_flip() +
  papaja::theme_apa() +
  theme(legend.position="none")
pl4

# Tabla APA estadísticos descriptivos
tbl_resumen_1 <- cleaned_data %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} [{median}] ({sd})", label = c(years_resid ~ "Años de residencia", env_home ~ "Sector", for_wet_pub ~ "Valoración")) %>%
  modify_header(label ~ "**Característica**") %>%
  modify_footnote(
    all_stat_cols() ~ "M [Mdn] (SD); n (%)"
  )
tbl_resumen_1 %>% as_flex_table() %>%
  save_as_docx(tbl_resumen_1, path = "tabla_descriptivos.docx")
tbl_resumen_1

##################################
#### Explorar la relación entre variables

# Relación entre opinión sobre uso de áreas naturales y sector de residencia

pl5 <- cleaned_data %>%
  ggplot(aes(x = env_home, fill = for_wet_pub)) +
  geom_bar(position = "fill") +
  labs(x = "Sector", y = "Proporción", fill = "Valoración") +
  ggtitle("Proporción \n Escala Likert por Sector") +
  papaja::theme_apa()
pl5

# Misma relación con porcentajes
tbl_2 <- cleaned_data %>% 
  group_by(env_home, for_wet_pub) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

pl6 <- tbl_2 %>%
  ggplot(aes(x = env_home, y = perc*100, fill = for_wet_pub)) +
  geom_bar(stat="identity") +
  labs(x = "Sector", y = "Porcentaje", fill = "Valoración") +
  ggtitle("Porcentaje \n Escala Likert por Sector") +
  papaja::theme_apa()
pl6

# Test chi-square para explorar relación entre variables categoricas

# Tabla de contingencia porcentajes muestra total
counts_table <- table(cleaned_data$env_home, cleaned_data$for_wet_pub)
prop.table(counts_table) %>% {. * 100} %>% 
  round(2)

# Tabla de contingencia condicional en relación al sector en porcentajes
prop.table(counts_table, 1) %>% {. * 100} %>% 
  round(2)

# Test chi-square
chisq.test(cleaned_data$env_home, cleaned_data$for_wet_pub, correct=FALSE)

# Relación entre años viviendo en un lugar y el sector en el que se vive
pl7 <- cleaned_data %>%
  ggplot(aes(x = years_resid, y = env_home)) +
  geom_boxplot(aes(colour = env_home)) +
  labs(x = "Años de residencia", y = "Sector", fill) +
  ggtitle("Distribución \n Años de residencia por Sector") +
  papaja::theme_apa() +
  theme(legend.position="none")
pl7

# Relación entre años viviendo en un lugar y la opinión sobre el uso público de bosques y humedales
pl8 <- cleaned_data %>%
  ggplot(aes(x = years_resid, y = for_wet_pub)) +
  geom_boxplot(aes(colour = for_wet_pub)) +
  labs(x = "Años de residencia", y = "Valoración", fill) +
  ggtitle("Distribución \n Años de residencia por Valoración") +
  papaja::theme_apa() +
  theme(legend.position="none")
pl8

# Relación entre opinión sobre uso de áreas naturales, sector de residencia, y años de residencia

pl9 <- cleaned_data %>%
  ggplot(aes(x = years_resid, y = env_home)) +
  geom_boxplot(aes(colour = env_home)) +
  labs(x = "Años de residencia", y = "Sector", fill) +
  facet_wrap(~for_wet_pub) +
  papaja::theme_apa() +
  theme(legend.position="none")
pl9


#############################
#### Realizar una regresión ordinal buscando responder a la pregunta de
# investigación. Se realiza esta regresión porque se busca determinar si
# la interacción de una variable nominal categórica y una variable continua
# tienen un efecto sobre una variable ordinal categórica.

# Comparar el modelo base con el modelo propuesto

# Modelo base o nulo: La opinión sobre el uso de humedales sin incluir ninguna
# variable predictiva
modelo_nulo <- clm(for_wet_pub ~ 1,
                   data = cleaned_data,
                   link = "logit")

# Modelo propuesto: La opinión sobre el uso de humedales predicha por
# el sector de residencia y/o los años que se lleva viviendo en dicho sector
modelo_1 <- clm(for_wet_pub ~ env_home * years_resid,
                data = cleaned_data,
                link = "logit")

#Comparación de modelos
anova(modelo_nulo, modelo_1)

# No hay una diferencia significativa entre los dos modelos,
# sin embargo, el modelo base o nulo presenta un mejor ajuste que el modelo propuesto basandonos en el AIC.

# Obtener valores de pseudo R cuadrado
nagelkerke(fit = modelo_1,
           null = modelo_nulo)

# Obtener coeficientes del modelo
summary(modelo_1)

# Ajustar el modelo con otro método para obtener los odds ratios
m <- polr(for_wet_pub ~ env_home * years_resid, data = cleaned_data, Hess = TRUE)
# Obtener coeficientes del modelo
summary(m)

# Crear la tabla de coeficientes para obtener los p-values
ctable <- coef(summary(m))

# Curva normal utilizada para obtener los p -valores
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# Tabla con coeficientes, interceptos y p-valores obtenidos con
# otro método para así corroborar los obtenidos con el primer método
ctable <- cbind(ctable, "p value" = p)
ctable

# Obtener intervalos de confianza
ci <- confint(m)
ci

# Convertir los coeficientes en odds ratios
exp(coef(m))
exp(cbind(OR = coef(m), ci))

# Probar que se cumple el supuesto del paralelismo
brant(m)

#Se cumple el supuesto

# Tabla APA regresión ordinal
tbl_logreg <- tbl_regression(modelo_1, exponentiate = TRUE,
                             label = c(years_resid ~ "Años de residencia", env_home ~ "Sector")) %>%
                              modify_header(label ~ "**Característica**")
tbl_logreg
