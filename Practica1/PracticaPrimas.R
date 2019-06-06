########################## DANIEL TOMÉ GORDO ##########################
############ MÁSTER EN DATA SCIENCE PARA FINANZAS (CUNEF) ############
############ DATA SCIENCE EN EL PRICING Y LA TARIFICACIÓN ############

# Instalación de librerías necesarias para la práctica. 
## Se pone una seed para la replicabilidad de los modelos

library(zoo)
library(caTools)
library(ROCR)
library(dplyr)

set.seed(666)


# Carga de los archivos necesarios

setwd("~/CUNEF/MASTER/2º CUATRIMESTRE/PRICING")
polizas <- read.csv2('Policy_v1.csv')
polizas2 <- read.csv2('Policy_v2.csv')
siniestros <- read.csv2('claims_v1.csv')

# Breve análisis exploratorio

summary(polizas) ## Se observan características bastante similares en todos los registros 
## Extrañan la edad mínima 17 años o el peso y valor 0 de algún coche
hist(polizas$Antigüedad, breaks = 10) ## Personas con bastante años de carnet
hist(polizas$Potencia) ## Coches con potencia entre los 100 y los 150 CV como se veía antes
hist(polizas$Peso) ## Otra vez entre los 1000 y 1500 kg la mayoría

summary(polizas2) ## Muchos de los datos son los mismos pero discretizados

summary(siniestros) ## Se reparten básicamente en daños propios y RC material con unos costes entorno a 900 euros

# Modelo de daños propios

DP_siniestros <- siniestros[siniestros$TipoSin %in% c("Daños Propios"),] # Se filtra por este tipo de daño
DP_costes <- data.frame(aggregate(DP_siniestros$Costes, by = list(DP_siniestros$ID_POL), mean)) # Se hace la media de los daños propios por cada póliza
DP_numero <- data.frame(aggregate(sign(DP_siniestros$Costes), by = list(DP_siniestros$ID_POL), sum)) # Se suman las veces que la póliza ha sufrido un evento
names(DP_costes) <- c("ID_POL", "Coste") # Se cambian los nombres
names(DP_numero) <- c("ID_POL", "N_sit") # Idem

DP_siniestros <- merge(DP_siniestros, DP_costes) # Se unen el df original con los costes medios por ID
DP_siniestros <- merge(DP_siniestros, DP_numero) # Se obtienen el df original, los costes medios por ID y la frecuencia de cada ID

summary(DP_siniestros)
hist(DP_siniestros$Coste)

# Modelo coste de daños propios
str(df_siniestro_coste)

df_siniestro_coste <- merge(DP_siniestros, polizas2) # Se unen los dataframes
df_siniestro_coste <- df_siniestro_coste[!duplicated(df_siniestro_coste$ID_POL), ] # Se eliminan los duplicados
df_siniestro_coste$Costes <- NULL # Se borra la columna costes, pues en coste ya aparece el coste (medio en el caso de los duplicados)

modelo_coste <- glm(Coste ~ Comb + Antigüedad + Forma_Pago + Edad_FMT + Valor_FMT + Potencia_FMT,
                    data = df_siniestro_coste, family = Gamma(link = 'log'))

summary(modelo_coste) ## Bastante buen modelo

polizas2 <- polizas2 %>%
  mutate(Pred_costeprima = predict(modelo_coste, newdata = polizas2, type = "response")) ## Se añade la columna con la parte de importe predicha

# Modelo de frecuencia 

df_siniestro_freq <- merge(DP_siniestros, polizas2)
df_siniestro_freq <- df_siniestro_freq[!duplicated(df_siniestro_freq$ID_POL), ] # Se eliminan los duplicados
df_siniestro_freq[is.na(df_siniestro_freq$N_sit),"N_sit"] <- 0 ## Se rellenan aquellos polizas sin accidentes a 0

summary(df_siniestro_freq) 

modelo_freq <- glm(N_sit ~ Comb + Antigüedad + Forma_Pago + Edad_FMT + Valor_FMT + Potencia_FMT,
                   data = df_siniestro_freq, family = poisson(link = "log")) # Se escogen las mismas que para el coste
summary(modelo_freq) ## Hay muchas variables que no son importantes

polizas2 <- polizas2 %>%
  mutate(Pred_freq = predict(modelo_freq, newdata = polizas2, type = "response"))

polizas2 <- polizas2 %>%
  mutate(Prima_DP = (Pred_freq * Pred_costeprima))

# Modelo final conjunto

modelo_prima <- glm(Prima_DP ~ Antigüedad + Edad_FMT + Valor_FMT + Comb + Potencia_FMT + Peso_FMT + Bonus_RC,
                    data = polizas2, family = Gamma(link = 'log'))

summary(modelo_prima) # Modelo muy bueno (no es de extrañar por los datos)

polizas2 <- polizas2 %>%
  mutate(Prima = predict(modelo_prima, newdata = polizas2, type = "response"))

exp(modelo_prima$coefficients) # Para la tarifa

# Perfil 1. Prima de 1448.50 euros

polizas2 %>% filter(Antigüedad == 8,Forma_Pago == 'A', Comb == 'D',Sexo == '2', Bonus_RC == '40',
                    Edad_FMT == '02.25-28', Valor_FMT == '02.10-16k', Potencia_FMT == '02.70-90', 
                    Peso_FMT == '03. 1200', Carnet_FMT == '03.5-7') 

# Perfil 2. Prima de 2508.52 euros
polizas2 %>% filter(Antigüedad == 10,Forma_Pago == 'A', Comb == 'D',Sexo == '1', Bonus_RC == '50',
                    Edad_FMT == '03.28-35', Valor_FMT == '06.30-35k', Potencia_FMT == '06.150-200',
                    Peso_FMT == '06. 1800', Carnet_FMT == '04.8-')

# Perfil 3. Prima de 699.18 euros

polizas2 %>% filter(Antigüedad == 0,Forma_Pago == 'A', Comb == 'D',Sexo == '1', Bonus_RC == '50',
                    Edad_FMT == '05.50-65', Valor_FMT == '01.<10k', Potencia_FMT == '02.70-90', 
                    Peso_FMT == '04. 1400', Carnet_FMT == '04.8-')

# Perfil 4. Prima de 4282.37 euros

polizas2 %>% filter(Antigüedad == 10, Forma_Pago == 'T', Comb == 'D', Sexo == '2', Bonus_RC == '10',
                    Edad_FMT == '01.18-25', Valor_FMT == '02.10-16k', Potencia_FMT == '03.90-110',
                    Peso_FMT == '03. 1200', Carnet_FMT == '01.0-1')

# Perfil 5. Prima de euros 993.49 euros

polizas2 %>% filter(Antigüedad == 10, Forma_Pago == 'A', Comb == 'D', Sexo == '1', Bonus_RC == '0',
                    Edad_FMT == '03.28-35', Valor_FMT == '04.20-24k', Potencia_FMT == '05.130-150',
                    Peso_FMT == '05. 1600', Carnet_FMT == '02.2-4') # No existe, por lo tanto se predecirá

# La fórmula sería: 2645.71 + 10*1.04 + 0*0.9986 * 0.3443 * 1.1664 * 0.92 * 1.012 = 993.49
