
##### 1. Bloque de inicializacion de librerias #####

if(!require("zoo")){
  install.packages("zoo")
  library("zoo")
}

if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd()
Fecha=Sys.Date()

## -------------------------------------------------------------------------


# fichero de polizas
Polizas=read.csv2('policy_v1.csv')


# fichero de polizas
Polizas2=read.csv2('policy_v2.csv')


##### 4. Bloque de analisis de Polizas #####

str(Polizas2)
summary(Polizas2)

hist(Polizas$Valor)
hist(Polizas$Potencia)
hist(Polizas$Peso)


table(Polizas$Forma_Pago)
table(Polizas$Antigüedad)
table(Polizas$Sexo)
table(Polizas$Edad,Polizas$Comb)

Polizas$start_date2<-as.Date(Polizas$start_date)
#Polizas$Expuestos=diff.Date(Polizas$end_date,Polizas$start_date)/365


# fichero de siniestros
Siniestros=read.csv2('claims_v1.csv')

str(Siniestros)
summary(Siniestros)

hist(Siniestros$Costes)


table(Siniestros$TipoSin)

#TP=RC-Corp Damage y Mat Damage
#Corp Damage
SINI_RCC=Siniestros[Siniestros$TipoSin %in% c("RC Corporal"),]
Costes=aggregate(SINI_RCC$Costes, by = list(SINI_RCC$ID_POL), mean)
Numero=aggregate(sign(SINI_RCC$Costes), by = list(SINI_RCC$ID_POL), sum)
Costes <- data.frame(ID_POL=Costes$Group.1, Costes=Costes$x)
Numer <- data.frame(ID_POL=Numero$Group.1,Nsini=Numero$x)
SINI_RCC=merge(SINI_RCC,Costes)
SINI_RCC=merge(SINI_RCC,Numer)
summary(SINI_RCC)

hist(SINI_RCC$Costes)


#Mat Damage
SINI_RCM=Siniestros[Siniestros$TipoSin %in% c("RC Material"),]
Costes=aggregate(SINI_RCM$Costes, by = list(SINI_RCM$ID_POL), mean)
summary(SINI_RCM)
Numero=aggregate(sign(SINI_RCM$Costes), by = list(SINI_RCM$ID_POL), sum)
Costes <- data.frame(ID_POL=Costes$Group.1, Costes=Costes$x)
Numer <- data.frame(ID_POL=Numero$Group.1,Nsini=Numero$x)
SINI_RCM=merge(SINI_RCM,Costes)
SINI_RCM=merge(SINI_RCM,Numer)
summary(SINI_RCM)

hist(SINI_RCM$Costes)


#Modelo coste de siniestros Corp Damage
RCC2 <- merge(SINI_RCC, Polizas2, by = "ID_POL")
RCC2 <- RCC2 %>% filter(Costes>0)
modelo2_C1=glm(Costes~Edad_FMT,data=RCC2,family=Gamma)
summary(modelo2_C1)
modelo2_C4=glm(Costes~Carnet_FMT+Forma_Pago,data=RCC2,
              family=Gamma)
summary(modelo2_C4)

Polizas2 <- Polizas2 %>% mutate(PredCorpDamage=predict(modelo2_C4,newdata = Polizas2,type = "response"))
Polizas2



#Modelo número de siniestros

RCCF <- merge(Polizas2,SINI_RCC, by = "ID_POL",all.x=TRUE)
RCCF[is.na(RCCF$Nsini),"Nsini"]<-0
RCCF <- RCCF %>% filter(Nsini>=0)
summary(RCCF)
summary(SINI_RCC)


ModeloN_C1=glm(Nsini~Edad_FMT+Valor_FMT+Sexo+Comb+
              Potencia_FMT+Peso_FMT+Bonus_RC,data=RCCF,
            family=poisson(link = "log"))
summary(ModeloN_C1)

Polizas2 <- Polizas2 %>% mutate(PredCorpDamageFRQ=predict(ModeloN_C1,newdata = Polizas2,type = "response"))
Polizas2 <- Polizas2 %>% mutate(Prima=(PredCorpDamage * PredCorpDamageFRQ))
summary(Polizas2$Prima)

#Modelo Prima de Riesgo/Final

ModeloN_prm=glm(Prima~Edad_FMT+Valor_FMT+Sexo+Comb+
                 Potencia_FMT+Peso_FMT+Bonus_RC,data=Polizas2,
               family=Gamma(link = "log"))
summary(ModeloN_prm)
summary(Polizas2)

# fichero de polizas
Polizas_fmt=read.csv2('policy_v2.csv')

str(Polizas_fmt)
summary(Polizas_fmt)




