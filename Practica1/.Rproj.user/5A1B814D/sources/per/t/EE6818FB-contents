#PRÁCTICA FINAL GESTIÓN DEL RIESGO OPERATIVO

##Alumno: JORGE CASAN VÁZQUEZ

library(MASS)
library(CASdatasets)
library(car)
library(actuar) # Librería utilizada para el análisis actuarial
library(fitdistrplus) #Librería utilizada para el ajuste de distribucones
library(ggplot2) # Librería utilizada para la visualización de gráficos
library(moments) # Librería utilziada para el cálculo de momentos (asimetría y curtosis)

data(danishuni) #Este es el dataset con que trabajaremos para realizar la práctica

###############ANALISIS EXPLORATORIO DE LOS DATOS ################################

head(danishuni)

summary(danishuni)
mean(danishuni$Loss)
var(danishuni$Loss)  
median(danishuni$Loss)


#Análisis de los cuartiles
quantile(danishuni$Loss,probs=c(0.05, 0.95))
quantile(danishuni$Loss,seq(0,1, 0.20)) 
quantile(danishuni$Loss,seq(0.9,1, 0.01))

hist(danishuni$Loss, pch=20,
     breaks=25, prob=TRUE, main="HISTOGRAMA",
     danishunilab =" N Loss", ylab = "Frecuencia") #Representamos a través de un histograma las pérdidas así como su frecuencia


boxplot(danishuni$Loss) #Valores muy concentrados en el primer cuartil y valores extremos muy dispersos en el último, lo cual es muy ASIMETRÍA
ggplot(danishuni) + geom_density(aes(danishuni$Loss))

skewness(danishuni$Loss) #El coeficiente de asimetría se inclina hacia la derecha. Indicar que es 18.78. Si fuese 0 no existiría asimetría alguna y nos encontraríamos ante una Distribución Normal.
kurtosis(danishuni$Loss) #Valores superiores a 3 indican que no siguen una distribucion Normal. Nuestros datos se comportan como una distribución Leptocúrtica



#####SELECCIÓN DEL MODELO: INFERENCIA PARAMETRICA #######################

library(dplyr)
library(lubridate)

#Voy a realizar diferentes histogramas agrupando por meses y semanas  para ver las la frecuencia en cuanto a severidades así como las frecuencias incurridas
##SEVERIDAD MENSUAL
severidad_mensual<- danishuni %>%
  group_by(month=floor_date(Date, "month")) %>% 
  summarize(amount=sum(Loss))

hist(severidad_mensual$amount, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA de severidad mensual",
     danishunilab =" N Loss", ylab = "Frecuencia")


##SEVERIDAD SEMANAL

severidad_semanal<- danishuni %>%
  group_by(month=floor_date(Date, "week")) %>% 
  summarize(amount=sum(Loss))

hist(severidad_semanal$amount, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA de severidad semanal",
     danishunilab =" N Loss", ylab = "Frecuencia")


##FRECUENCIA MENSUAL

frecuencia_mensual<- danishuni %>%
  group_by(month=floor_date(Date, "month")) %>% 
  summarize(n=n())

hist(frecuencia_mensual$n, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA de frecuencia mensual",
     danishunilab =" N Loss", ylab = "Frecuencia")


#FRECUENCIA SEMANAL

frecuencia_semanal<- danishuni %>%
  group_by(month=floor_date(Date, "week")) %>% 
  summarize(n=n())

hist(frecuencia_semanal$n, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA de frecuencia semanal",
     danishunilab =" N Loss", ylab = "Frecuencia")


#POISSON Y BINOMIAL NEGATIVA MENSUAL
#Realizando el análisis para Poisson y la Binomial negativa podemos hacernos una idea aproximada si la evidencia empírica se relaciona con la teórica para la validación del modelo
#Para ello tendremos por una parte la función de densidad a la izquierda y a la derecha la función de distribución acumulada
#Realizaré este análisis con la frecuencia mensual y la semanal y compararé los resultados obtenidos

fpois=fitdist(frecuencia_mensual$n, "pois")
fpois
plot(fpois)


fnbinom=fitdist(frecuencia_mensual$n, "nbinom")
fnbinom
plot(fnbinom)


gofstat(list(fpois, fnbinom), chisqbreaks=c(0:4, 9), discrete=TRUE,
        fitnames=c("Poisson", "Binomial Negativa"))


#POISSON Y BINOMIAL NEGATIVA SEMANAL

fpois=fitdist(frecuencia_semanal$n, "pois")
fpois
plot(fpois)


fnbinom=fitdist(frecuencia_semanal$n, "nbinom")
fnbinom
plot(fnbinom)


gofstat(list(fpois, fnbinom), chisqbreaks=c(0:4, 9), discrete=TRUE,
        fitnames=c("Poisson", "Binomial Negativa"))

#Existe un mayor ajuste MENSUAL que SEMANAL, no solamente para la función de densidad sino también para la función de distribución acumulada
#Comparando los resultados obtenidos observamos que la Binomial negativa realiza un mejor ajuste que la Poisson debido a que el AIC y el BIC resulta inferior
# El estadístico de contrate es la Chi-cuadrado, ajustando los valores al modelo realizando un contraste de hipótesis en donde la hipótesis nula es la asunción de Normalidad en la distribución 
#Los Chi-breaks que establezco son  de (0:4, y 9) ya qye considero que son los valores que mejor definen los valores muestrales. Este criterio resulta determinante a la hora de calcular el estadístico de la Chi-cuadrado

#En base a todos estos criterios el mejor resultado lo ofrece la Binomial Negativa agrupando por frecuencias mensuales


#####ANALISIS DE VALORES EXTREMOS

##DISTRIBUCIÓN GENERALIZADA DE PARETO

#Es importante destacar que la distribución Gamma se utiliza para modelar variables que describen el tiempo hasta que se produce p veces un determinado suceso
#Cuenta con dos parámetros positivos, la forma y la escala

fgam<-fitdist(danishuni$Loss, 'gamma',lower=0)
fgam
plot(fgam)

fpar<- fitdist(danishuni$Loss, 'pareto', start= list(shape=2, scale=2), lower= 0)
fpar
plot(fpar)


dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1-prob)*dpareto(x, alpha, theta)
pmixgampar <- function(q, prob, nu, lambda, alpha, theta) 
  prob*pgamma(q, nu, lambda) + (1-prob)*ppareto(q, alpha, theta)


fmixgampar <- fitdist(danishuni$Loss, "mixgampar",
                      start=list(prob=1/2, nu=1, lambda=1, alpha=2,
                                 theta=2), lower=0)
fmixgampar



cbind(SINGLE= c(NA, fgam$estimate, fpar$estimate), 
      MIXTURE=fmixgampar$estimate)


fburr <- fitdist(danishuni$Loss, "burr", start=list(shape1=2, shape2=2, scale=2),
                 lower=c(0.1,1/2, 0))
fburr$estimate


FDD=cdfcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE,
            ylab = "Probabilidad", datapch=".",
            datacol="red", fitcol="black", fitlty=2:5,
            legendtext=c("gamma","Pareto", "Par-gam", "Burr"),
            main="Ajuste perdidas legales", plotstyle = "ggplot")
FDD

#El objetivo es determinar la existencia de colas pesadas en nuestra distribución de probabilidad y estimar los parámetros  asociados a estas distribuciones a través de la distribución generalziada de Pareto
#Representamos un histograma para seleccionar la distribución

hist(danishuni$Loss, pch=10, breaks=100, prob=TRUE, main="PERDIDAS LEGALES EN CORONAS DANESAS",
     xlab =" X", ylab = "Densidad")

curve(dburr(x, fburr$estimate[1],fburr$estimate[2], fburr$estimate[3]),
      col="red", lwd=2, add=T)


qqnorm(danishuni$Loss)
qqline(danishuni$Loss, col="red")


qmixgampar <- function(p, prob, nu, lambda, alpha, theta)
{L2 <- function(q, p) 
  (p - pmixgampar(q, prob, nu, lambda, alpha, theta))^2 
sapply(p, function(p) optimize(L2, c(0, 10^3), p=p)$minimun)
}



ppcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="probabilidades empiricas", xlab="probabilidades teoricas", fitcol="blue",
       main="PP-plot sobre perdidas", legendtext=c("gamma", "Pareto", "Par-gam", "Burr"),
       plotstyle = "ggplot", fitpch=1:4)

#La que mejor ajusta es la Burr con lo teórico, en cambio la distribución empírica de Pareto es la que menos con respecto a la teórica

qqcomp(list(fgam, fpar, fmixgampar, fburr), xlogscale=TRUE, ylogscale=TRUE, 
       ylab="cuantiles empiricos", xlab="cuantiles teoricos",
       fitcol="black", main="QQ-plot sobre perdidas", addlegend = TRUE,
       legendtext=c("gamma", "Pareto","Par-gam", "Burr"), fitpch=1:4)

######DISTRIBUCIÓN GENERALIZADA DE VALORES EXTREMOS###########################

library(evmix)
#Destacar que el método de la teoría de Valores Extremos (EVT) se utiliza para modelar excesos sobre un umbral
#Valores extremos de severidad

#La modelización de las colas puede desarrollarse a través de diferentes estrategias. Por una parte, el POT ofrece un análisis de los valores extremos a partir del análisis de los valores que exceden cierto umbral
# y por otra parte, la distribución de valores extremos generalizados (GEV) se ajusta mediante Block Máxima, la cual se ajusta en la distribución de valores máximos o mínimos

# Exceso sobre umbral. Extrac. de Valores que exceden un umbral
u <- 50
danish.exc <- danishuni[danishuni[,2] > u, 2]
danish.exc
summary(danish.exc)

#Visualizacion de las colas

n.u <- length(danish.exc) #n? de casos que superan u
surv.prob <- 1 - rank(danish.exc)/(n.u + 1)  #rank ofrece el n? de orden
surv.prob

plot(danish.exc, surv.prob, log = "xy", xlab = "Excesos", 
     ylab = "Probabilidades", ylim=c(0.01, 1))


#Añadimos las prob teoricas de la D.Pareto

#Determinamos los parámetros necesarios
alpha <- - cov(log(danish.exc), log(surv.prob)) / var(log(danish.exc))
alpha 

x = seq(u, max(danish.exc), length = 100) #divide de u a max() 100 interv.
y = (x / u)^(-alpha)
lines(x, y)

#Funcion de distribuci?n acumulada
prob <- rank(danish.exc) / (n.u + 1)
plot(danish.exc, prob, log = "x", xlab= "Excesos", ylab = "Probabilidades de no excesos")
y = 1 - (x / u)^(-alpha)
lines(x, y)

#Dist. valores extremos generalizados (GEV)

nllik.gev <- function(par, data){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  n <- length(data)
  if (xi == 0)
    n * log(sigma) + sum((data - mu) / sigma) +
    sum(exp(-(data - mu) / sigma))
  else {
    if (any((1 + xi * (data - mu) / sigma) <= 0))
      return(1e6)
    n * log(sigma) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - mu) / sigma)) +
      sum((1 + xi * (data - mu) / sigma)^(-1/xi))
  }
}

# Datos de perdidas
danish.claim <- danishuni[,2]
danish.claim


#Block Maxima: Extrac. de los valores max de cada año

#Extrar las cuatro primeros valores de la variable
years <- as.numeric(substr(danishuni[,1], 1, 4))
years
danish.max <- aggregate(danish.claim, by=list(years), max, na.rm=TRUE)[,2]
danish.max

sigma.start <- sqrt(6) * sd(danish.max) / pi
mu.start <- mean(danish.max) + digamma(1) * sigma.start
fit.gev <- nlm(nllik.gev, c(mu.start, sigma.start, 0),
               hessian = TRUE, data = danish.max)
fit.gev
fit.gev$estimate #par.posicion, escala y forma

sqrt(diag(solve(fit.gev$hessian))) 


#Modelo Poisson-Generalizada de Pareto 

nllik.gp <- function(par, u, data){
  tau <- par[1]
  xi <- par[2]
  if ((tau <= 0) | (xi < -1))
    return(1e6)
  m <- length(data)
  if (xi == 0)
    m * log(tau) + sum(data - u) / tau
  else {
    if (any((1 + xi * (data - u) / tau) <= 0))
      return(1e6)
    m * log(tau) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - u) / tau))
  }
}


u <- 10
tau.start <- mean(danish.exc) - u
fit.gp <- nlm(nllik.gp, c(tau.start, 0), u = u, hessian = TRUE,
              data = danish.exc)
fit.gp
fit.gp$estimate 


#Intervalo de confianza del valor m?ximo del indice de cola (si=0,50)
# al 95%
prof.nllik.gp <- function(par,xi, u, data)
  nllik.gp(c(par,xi), u, data)
prof.fit.gp <- function(x)
  -nlm(prof.nllik.gp, tau.start, xi = x, u = u, hessian = TRUE,
       data = danish.exc)$minimum
vxi = seq(0,1.8,by=.025)
prof.lik <- Vectorize(prof.fit.gp)(vxi)
plot(vxi, prof.lik, type="l", xlab = expression(xi),
     ylab = "Profile log-likelihood")
opt <- optimize(f = prof.fit.gp, interval=c(0,3), maximum=TRUE)
opt


up <- opt$objective
abline(h = up, lty=2)
abline(h = up-qchisq(p = 0.95, df = 1), col = "grey")
I <- which(prof.lik >= up-qchisq(p = 0.95, df = 1))
lines(vxi[I], rep(up-qchisq(p = 0.95, df = 1), length(I)),
      lwd = 5, col = "grey")
abline(v = range(vxi[I]), col = "grey", lty = 2)
abline(v = opt$maximum, col="grey")


#Point Process, o para valores err?ticos
nllik.pp <- function(par, u, data, n.b){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  if (xi == 0)
    poiss.meas <- n.b * exp(-(u - mu) / sigma)
  else
    poiss.meas <- n.b * max(0, 1 + xi * (u - mu) / sigma)^(-1/xi)
  exc <- data[data > u]
  m <- length(exc)
  if (xi == 0)
    poiss.meas + m * log(sigma) + sum((exc - mu) / sigma)
  else {
    if (any((1 + xi * (exc - mu) / sigma) <= 0))
      return(1e6)
    poiss.meas + m * log(sigma) + (1 + 1 / xi) *
      sum(log(1 + xi * (exc - mu) / sigma))
  }
}

n.b <- 1991 - 1980
u <- 10
sigma.start <- sqrt(6) * sd(danish.exc) / pi
mu.start <- mean(danish.exc) + (log(n.b) + digamma(1)) *
  sigma.start


fit.pp <- nlm(nllik.pp, c(mu.start, sigma.start, 0), u = u,
              hessian = TRUE, data = danishuni[,2], n.b = n.b)
fit.pp

#Estimacion otros indices de cola

#Intervalo confianza asintotico del indice de cola

logXs <- log(sort(danishuni[,2], decreasing=TRUE))
n <- length(logXs)
xi <- 1/1:n * cumsum(logXs) - logXs
ci.up <- xi + qnorm(0.975) * xi / sqrt(1:n)
ci.low <- xi - qnorm(0.975) * xi / sqrt(1:n)
matplot(1:n, cbind(ci.low, xi, ci.up),lty = 1, type = "l",
        col = c("blue", "black", "blue"), ylab = expression(xi),
        xlab = "Numero valores extremos")


#Intervalo confianza de alfa=1/indice de cola

alpha <- 1 / xi
alpha.std.err <- alpha / sqrt(1:n)
ci.up <- alpha + qnorm(0.975) * alpha / sqrt(1:n)
ci.low <- alpha - qnorm(0.975) * alpha / sqrt(1:n)
matplot(1:n, cbind(ci.low, alpha, ci.up), lty = 1, type = "l",
        col = c("blue", "black", "blue"), ylab = expression(alpha),
        xlab = "Numero valores extremos")



#Exceso sobre la media

meanExcessPlot <- function(data, u.range = range(data),n.u = 100){
  mean.excess <- ci.up <- ci.low <- rep(NA, n.u)
  all.u <- seq(u.range[1], u.range[2], length = n.u)
  for (i in 1:n.u){
    u <- all.u[i]
    excess <- data[data > u] - u
    n.u <- length(excess)
    mean.excess[i] <- mean(excess)
    var.mean.excess <- var(excess)
    ci.up[i] <- mean.excess[i] + qnorm(0.975) *
      sqrt(var.mean.excess / n.u)
    ci.low[i] <- mean.excess[i] - qnorm(0.975) *
      sqrt(var.mean.excess / n.u)
  }
  matplot(all.u, cbind(ci.low, mean.excess, ci.up), col = 1,
          lty = c(2, 1, 2), type = "l", xlab = "u", ylab = "Exc.sobre media")
}
meanExcessPlot(danish.exc)

#### VALIDACION DEL MODELO

#Q-Q Plot para la Dist. Generalizada de Pareto (DGP)

qqgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  prob <- 1:m / (m + 1)
  x.hat <- u + tau / xi * ((1 - prob)^-xi - 1)
  ylim <- xlim <- range(x.hat, excess)
  plot(sort(excess), x.hat, xlab = "Quantiles en la muestra",
       ylab = "Quantiles ajustados", xlim = xlim, ylim = ylim)
  abline(0, 1, col = "grey")
}

qqgpd(danishuni[,2], 10, 7, 0.5)#u=10, tau=7 y indice cola=0,5

#P-P Plot para la Dist. Generalizada de Pareto (DGP)

ppgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  emp.prob <- 1:m / (m + 1)
  prob.hat <- 1 - (1 + xi * (sort(excess) - u) / tau)^(-1/xi)
  plot(emp.prob, prob.hat, xlab = "Probabilidades empiricas",
       ylab = "Probabilidades ajustadas", xlim = c(0, 1),
       ylim = c(0, 1))
  abline(0, 1, col = "grey")
}

ppgpd(danishuni[,2], 10, 7, 0.5) #u=10, tau=7 y indice cola=0,5

#Validamos las probabilidades empíricas obtenidas con las probabilidades ajustadas y vemos que el impacto producido por las pérdidas teóricas es capaz de explicar las pérdidas incurridas

######## COMUNICACIÓN DE RESULTADOS#####################

#A fin de comunicar nuestros resultados resulta fundamental realizar un análisis de las pérdidas agregadas
#Para ello basaremos nuestro análisis por medio de dos vías. Por una parte, a través del modelo Gamma en donde comprobaremos que la mejor aproximación de pérdidas agregadas la ofrece la Normal-Power y por otra parte,  a través del método de Pareto en donde calcularemos la función de distribución discretizándola

library(actuar)

#El codigo implementa una Gamma, con "n" v.a. Xi distribuidas como Gamma(n*shape, rate)


pgamsum <- function(x, dfreq, argfreq, shape, rate, Nmax=10)
{
  tol <- 1e-10; maxit <- 10
  nbclaim <- 0:Nmax
  dnbclaim <- do.call(dfreq, c(list(x=nbclaim), argfreq))
  psumfornbclaim <- sapply(nbclaim, function(n)
    pgamma(x, shape=shape*n, rate=rate))
  psumtot <- psumfornbclaim %*% dnbclaim
  dnbclaimtot <- dnbclaim
  iter <- 0
  while( abs(sum(dnbclaimtot)-1) > tol && iter < maxit)
  {
    nbclaim <- nbclaim+Nmax
    dnbclaim <- do.call(dfreq, c(list(x=nbclaim), argfreq))
    psumfornbclaim <- sapply(nbclaim, function(n)
      pgamma(x, shape=shape*n, rate=rate))
    psumtot <- psumtot + psumfornbclaim %*% dnbclaim
    dnbclaimtot <- c(dnbclaimtot, dnbclaim)
    iter <- iter+1
  }
  as.numeric(psumtot)
}

#CASO 1: GAMMA

#N sigue una Poisson(10) y X sigue una Gamma(3,2)

parsev <- c(3, 2); parfreq <- 10 #Fijamos parametros v.a. severidad y frecuencia

meansev <- mgamma(1, parsev[1], parsev[2]) #Momento de orden 1 gamma
varsev <- mgamma(2, parsev[1], parsev[2]) - meansev^2 #Momento de orden 2 gamma
skewsev <- (mgamma(3, parsev[1], parsev[2]) -
              3*meansev*varsev - meansev^3)/varsev^(3/2) #Coef.Asimetria gamma

meanfreq <- varfreq <- parfreq[1]; skewfreq <- 1/sqrt(parfreq[1]) #Momento de orden 1 Poisson

meanagg <- meanfreq * meansev # Momento 1 Varaible agregeda

varagg <- varfreq * (varsev + meansev^2) # Varianza v. agregada

skewagg <- (skewfreq*varfreq^(3/2)*meansev^3 + 3*varfreq*meansev*
              varsev + meanfreq*skewsev*varsev^(3/2))/varagg^(3/2) # Coef.asimetria agre

######### Funci?n de distribucion de la perdidas agregadas F(s)

#Proyecto de simulaci?n

F.s <- aggregateDist("simulation", model.freq = expression(y =rpois(parfreq)),
                     model.sev = expression(y =rgamma(parsev[1], parsev[2])),
                     nb.simul = 1000)


#Aproximaci?n a trav?s de Normal

F.n <- aggregateDist("normal", moments = c(meanagg, varagg))


#Aproximaci?n a trav?s de normal-power

F.np <- aggregateDist("npower", moments = c(meanagg, varagg, skewagg))


#Funcion exacta
F.exact <- function(x) pgamsum(x, dpois, list(lambda=parfreq),
                               parsev[1], parsev[2], Nmax=100)


x <- seq(25,40) #Cambiar a 25,40

plot(x, F.exact(x), type="l",
     main="Distribuci?n Agregada de p?rdidas", ylab="F(x)")
lines(x, F.s(x), lty=2, col = "red")
lines(x, F.n(x), lty=3,col = "blue" )
lines(x, F.np(x), lty=4, col = "green")
legend("bottomright", leg=c("exacta", "simulacion",
                            "Aprox.normal", "Approx.NP"),
       col = c("black", "red", "blue", "green"),
       lty = 1:4, text.col = "black")


#CASO DE LA DISTRIBUCION PARETO

#N sigue una Pareto (10) y X sigue una gamma (3,2)

parsev <- c(3.1, 2*2.1) ; parfreq <- 10

xmax <- qpareto(1-1e-9, parsev[1], parsev[2]) #Func.cuantialica  


#Calculamos funcion de distribucion discretizandola (funcion discretize)

#Discretizacion por M. recursivo

#Metodo insesgado
fx2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
                  to = xmax, step = 0.5, method = "unbiased",
                  lev = levpareto(x, parsev[1], parsev[2])) #unbiased# metodo centrado


F2 <- aggregateDist("recursive", model.freq = "poisson",
                    model.sev = fx2, lambda = parfreq, x.scale = 0.5, maxit=2000)

#Metodo d. superior
fx.u2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
                    to = xmax, step = 0.5, method = "upper")

F.u2 <- aggregateDist("recursive", model.freq = "poisson",
                      model.sev = fx.u2, lambda = parfreq, x.scale = 0.5, maxit=2000)

#Metodo d. inferior
fx.l2 <- discretize(ppareto(x, parsev[1], parsev[2]), from = 0,
                    to = xmax, step = 0.5, method = "lower")

F.l2 <- aggregateDist("recursive", model.freq = "poisson",
                      model.sev = fx.l2, lambda = parfreq, x.scale = 0.5, maxit=2000)



x <- seq(50, 120)
plot(x, F2(x), type="l",
     main="Distribuci?n Agregada de p?rdidas", ylab="F1(y)")

lines(x, F.u2(x), lty=2, col="red")

lines(x, F.l2(x), lty=3, col="blue")


legend("bottomright", leg=c("Rec.insesgado", "Rec.superior", "Rec.inferior"),
       col = c("black", "red", "blue"),
       lty = 1:3, text.col = "black")



#####APÉNDICE: MEDICIÓN DEL RIESGO EXTREMO (VaR)   VALUE AT RISK ###########################################

alpha <- c(0.05)
q=qnorm(alpha, mean=0.05, sd=0.18) 
q


R=q*20000 #Perdida con rentabilidad negativa
R

Var=-R  #VaR,cambia el signo a R, para que resulte valor positivo
Var

## CASO 1

#  VaR
alpha <- c(0.95, 0.99)
qnorm(alpha, mean=-0.05, sd=0.18) * 20000


# Tambien cambiando de signo a la media

(-0.05 + 0.18 * qnorm(alpha)) * 20000


# ES

(-0.05 + 0.18 * dnorm(qnorm(alpha))/(1 - alpha)) * 20000


# Representamos ambos casos

x <- seq(0.9,0.999, length=100)

yVaR <- (-0.05 + 0.18 * qnorm(x)) * 20000
yES <- (-0.05 + 0.18 * dnorm(qnorm(x))/(1 - x)) * 20000

plot(x, yVaR, type="l", ylim=range(yVaR, yES),
     xlab=expression(alpha), ylab="")

lines(x, yES, lty=2, col=2)
legend("topleft", legend=c("VaR","ES"),col=1:2, lty=1:2)



## CASO 2

library("sn")  #Para trabajar con la distribucion t


# a) VaR
alpha <- c(0.95,0.99)
mu <- 0.05
lambda <- 0.116
nu <- 5
q <- qst(alpha, location=-mu, scale=lambda, df=nu)

q * 20000


# ES
f <- dt(qt(alpha, df=nu), df=nu)
(-mu + lambda * f / (1-alpha) * (nu + qt(alpha, df=nu)^2) / (nu-1) ) * 20000

## CASO 3 

## Fijamos las fecha del analisis
sp.end <- as.Date("2019-03-31")
(sp.start <- sp.end - 1000)

## Tomamos datos de SP500

library("tseries") #la libreria permite descargar los datos de cotizacion
x <- get.hist.quote(start=sp.start, end = sp.end, instrument = "^gspc",
                    provider = "yahoo", quote="Close")


nrow(x)   # son 1000? 

#Fijamos la fecha comienzo de descarga

x <- get.hist.quote(start="2000-01-01", end = sp.end,
                    instrument = "^gspc",
                    provider = "yahoo", quote="Close")

x <- tail(x, 1000)  #Tama?o de la cola 

nrow(x) # Ahora son 1000?


plot(x, xlab="S&P 500", ylab="level")
grid() #A?adimos cudricula

## b)

library("zoo")  #libreria para series temporales

head(x)

#diff, calcula la tasa de variacion logaritmica de los precios = rentabilidad

sp500.ret.zoo <- diff(log(x))
head(sp500.ret.zoo)

sp500.ret <- as.numeric(sp500.ret.zoo)

hist(sp500.ret, breaks=50, freq=FALSE)
alpha <- 0.95
q<- quantile(sp500.ret, probs=1-alpha) 
q

abline(v=q, col=2)

## c)
## VaR empirico (Simulacion historica)

q* (-20000)

## ES empirico (Simulacion historica)

mean(sp500.ret[sp500.ret < q]) * (-20000)

## d) IC bootstrap 

set.seed(1234)
B <- 10000

## Utilizando la libreria "boot"

library(boot)
VaRES <- function(x,i){
  q <- quantile(x[i], probs=1-alpha)   
  c(-q * 20000, -mean(x[x < q]) * 20000)
}

res<-boot(sp500.ret,statistic=VaRES, R=B, stype="i")
# stype="i", para tipo de caracteres indice (pueden ser frecuencias, pesos,..)

# Calulo del Intervalo de Confinza

boot.ci(res, conf=0.95, type="perc", index=1) 
boot.ci(res, conf=0.95, type="perc", index=2)# 2 ES

## e) Representacion grafica

plot(sp500.ret.zoo)
abline(h=q, col="red")
points(sp500.ret.zoo[sp500.ret < q], pch=20, col=2)




























