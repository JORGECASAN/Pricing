#PRÁCTICA FINAL DE PRICING Y TARIFICACIÓN ESPECIAL

##JORGE CASAN VÁZQUEZ

#Nos instalamos todas las librerías necesarias para poder realizar la práctica
library(zoo)
library(caTools)
library(ROCR)
library(dplyr)

#Esta es la fecha actual
Fecha=Sys.Date()

#Nos cargamos los dos ficheros respectivos sobre las Pólizas

Polizas=read.csv2('Policy_v1.csv')
Polizas2= read.csv2('Policy_v2.csv')

#Realizamos un breve análisis exploratorio de los datos para ver su composición

str(Polizas) #Vemos en qué formato están nuestras variables
str(Polizas2) #Lo mismo hacemos con el formato del siguiente archivo

summary(Polizas) #Nos proporciona información relevante sobre cómo está la distribución en los datos
summary(Polizas2) # Además ,el summary nos ofrece información relevante sobre variables numéricas

hist(Polizas$Potencia) #La mayoría de la potencia de los coches está entre 100 y 200 CV
hist(Polizas$Valor) # La mayoría de los coches tienen un valor de 16.000 € aproximadamente.
hist(Polizas$Peso)#La mayoría de los coches tiene un peso comprendido entre 1.000 y 2.000 kgs.

hist(Polizas2$Expuestos) #Vemos como el existen valores atípicos y como la frecuencia se distribuye hacia la derecha en la distribución

table(Polizas$Forma_Pago) #La mayoría de la forma de pago elegida por los asegurados es la forma A
table(Polizas$Antigüedad) #Existe gran volúmen de asegurados que tiene bastante tiempo el carnet de coche
table(Polizas$Sexo) # Más del doble de los asegurados son hombres

#Nos creamos una columna adicional que represente la fecha de originicación de la Póliza
Polizas$start_date2<- as.Date(Polizas$start_date)

#Nos cargamos el fichero de los siniestros

siniestros<- read.csv2('Claims_v1.csv')

str(Siniestros)
summary(Siniestros)

hist(Siniestros$Costes) #De nuevo tenemos que el coste de los siniestros es mucho inferior a los 10.000€

table(Siniestros$TipoSin)# La mayoría de la naturaleza de los siniestros son por RC Material y por Daños Propios

#Para realizar el cálculo de la cobertura sobre los Daños Propios realizamos el modelo coste de siniestros, el modelo del 
#número de siniestros  y el modelo Prima de Riesgo Final

# 1-) Modelo coste de siniestros

DP_siniestros <- siniestros[siniestros$TipoSin %in% c("Daños Propios"),] # Hacemos el filtro por los daños propios
DP_costes <- data.frame(aggregate(DP_siniestros$Costes, by = list(DP_siniestros$ID_POL), mean)) # Se hace la media de los daños propios por cada póliza
DP_numero <- data.frame(aggregate(sign(DP_siniestros$Costes), by = list(DP_siniestros$ID_POL), sum)) # Se suman las veces que la póliza ha sufrido un evento
names(DP_costes) <- c("ID_POL", "Coste") # Cambiamos los nombres
names(DP_numero) <- c("ID_POL", "N_sit") # Hacemos lo mismo para el número de siniestros

DP_siniestros <- merge(DP_siniestros, DP_costes) # Se unen el df original con los costes medios por ID
DP_siniestros <- merge(DP_siniestros, DP_numero) # Se obtienen el df original, los costes medios por ID y la frecuencia de cada ID

summary(DP_siniestros) #Podemos observar la diferencia entre la media y la mediana, lo cual indica la presencia de outliers. 
#El número de costes centrado (la mediana) es el importe de 364 € siendo el valor máximo 14.487 €

hist(DP_siniestros$Coste)
hist(DP_siniestros$N_sit)

DP2<- merge(DP_siniestros, Polizas2, by='ID_POL')

DP2<- DP2[!duplicated(DP2$ID_POL),] #Eliminamos los duplicados
DP2$Costes <- NULL #Nos interesa la columna antes nombrada como 'Coste', por lo que para no incurrir en multiolinealidad
#eliminamos la columna 'Costes'

modelo1 <- glm(Coste ~ Comb + Antigüedad + Forma_Pago + Edad_FMT + Valor_FMT + Potencia_FMT,
                    data = DP2, family = Gamma(link = 'log'))


summary(modelo1)# El intercept, la Antiguedad y Valor_FMT07.35-40k son los estadísticamente más significativos
#El AIC resultante es 16283 y el número de iteraciones Fisher de 10
coefficients(modelo1)

Polizas2 <-Polizas2 %>% mutate(Pred_costeprima = predict(modelo1, newdata = Polizas2, type = "response"))
#Realizamos la predicción sobre el coste de la prima

set.seed(123)

names(DP2)

#2-) Modelo número de siniestros

df_siniestro_freq <- merge(DP_siniestros, Polizas2)
df_siniestro_freq <- df_siniestro_freq[!duplicated(df_siniestro_freq$ID_POL), ] # Se eliminan los duplicados
df_siniestro_freq[is.na(df_siniestro_freq$N_sit),"N_sit"] <- 0 ## Se rellenan aquellos polizas sin accidentes a 0

summary(df_siniestro_freq) 

modelo_freq <- glm(N_sit ~ Comb + Antigüedad + Forma_Pago + Edad_FMT + Valor_FMT + Potencia_FMT,
                   data = df_siniestro_freq, family = poisson(link = "log")) # Se escogen las mismas que para el coste
#Esta vez en family en vez de escoger la Gamma escogemos la Possion puesto que estamos calculando frecuencias
summary(modelo_freq) # Hay muchas variables que no son importantes

Polizas2 <- Polizas2 %>%
  mutate(Pred_freq = predict(modelo_freq, newdata = Polizas2, type = "response"))

# 3-) Modelo Prima Riesgo Final

Polizas2 <- Polizas2 %>%
  mutate(Prima_final = (Pred_freq * Pred_costeprima))

modelo_prima<- glm(Prima_final~Edad_FMT+Valor_FMT+Sexo+Comb+Potencia_FMT+Peso_FMT+Bonus_RC,data=Polizas2, 
                   family = poisson(link='log'))

summary(modelo_prima) #La mayoría de los estadisticos son significativos

#Para calcular la prima de riesgo final para los 5 perfiles

##1º PERFIL - Importe de la prima: 1382.163 €

 Polizas2 %>% filter(Antigüedad==8,Forma_Pago=='A',Comb=='D',Sexo=='2', Bonus_RC=='40' ,Edad_FMT=='02.25-28',
                    Valor_FMT=='02.10-16k', Potencia_FMT=='02.70-90', Peso_FMT=='03. 1200',Carnet_FMT=='03.5-7')


## 2º PERFIL -Importe de la prima: 2361.904 €
 
 Polizas2 %>% filter(Antigüedad==10,Forma_Pago=='A',Comb=='D',Sexo=='1', Bonus_RC=='50' ,Edad_FMT=='03.28-35',
                     Valor_FMT=='06.30-35k', Potencia_FMT=='06.150-200', Peso_FMT=='06. 1800',Carnet_FMT=='04.8-')
 
 
 ## 3º PERFIL - Importe de la prima: 682.3678 €
 
Polizas2 %>% filter(Antigüedad==0,Forma_Pago=='A',Comb=='D',Sexo=='1', Bonus_RC=='50' ,Edad_FMT=='05.50-65',
                     Valor_FMT=='01.<10k', Potencia_FMT=='02.70-90', Peso_FMT=='04. 1400',Carnet_FMT=='04.8-')

 ## 4º PERFIL -Importe de la prima: 4419.227 €

Polizas2 %>% filter(Antigüedad==10,Forma_Pago=='T',Comb=='D',Sexo=='2', Bonus_RC=='10' ,Edad_FMT=='01.18-25',
                    Valor_FMT=='02.10-16k', Potencia_FMT=='03.90-110', Peso_FMT=='03. 1200',Carnet_FMT=='01.0-1')
 
## 5º PERFIL -Importe de la prima: NOT FOUND

Polizas2 %>% filter(Antigüedad==10,Forma_Pago=='A',Comb=='D',Sexo=='1', Bonus_RC=='0', Edad_FMT=='03.28-35',
                    Valor_FMT=='04.20-24k', Potencia_FMT=='05.130-150', Peso_FMT=='05. 1600',Carnet_FMT=='02.2-4')


#Para calcular la tarifa final 


exp(modelo_prima$coefficients)

#1º perfil: 3925.2764541 + 0.9983080*50 + 0.9777285*1+  0.3099418*4 +  0.8939432*1 + 1.0366695*3 = 4.031 € 
# 2º perfil: 3925.2764541 + 1*0.9777285 + 50*0.9983080 + 0.3443393* 2 + 0.8862635*5 + 1.0779840*5 = 4036,64
# 3º perfil: 3925.2764541 + 1*0.9777285 + 50*0.9983080 + 0.3099418*4 + 0.8939432*1 + 1.0205512*3= 4.494,14 €
# 4º perfil: 3925.2764541 + 2*0.9777285 + 10*0.9983080 +  1.0286278*3 + 0.8612967*2 + 1.0205512*2= 3.944,07 € 
#5º perfil: 3925.2764541 + 1*0.9777285 + 0* 0.9983080 +  0.3443393*2 + 1.0286278*1 + 0.8527575*1 = 3928,81 €
