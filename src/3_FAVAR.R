library(tidyverse)
library(readxl)
library(forecast)
library(janitor)
library(vars)

df_forecast <- read_excel("data/forecast_DATA.xlsx") %>% 
  clean_names()

# Selecciono solo los datos de USA 
df_usa <- df_forecast %>% 
  dplyr::select(date, ends_with("us"), comprice)

# Traigo todos los IPC
lista <- c("cpiqus","cpiqca","cpiquk","cpiqjp","cpiqde","cpiqfr","cpiqit",
           "cpiqat","cpiqbe","cpiqdk","cpiqfi","cpiqgr","cpiqie","cpiqlu",
           "cpiqpt","cpiqes","cpiqse","cpiqnl","cpiqau","cpiqnz","cpiqno","cpiqch")

#Selecciono IPC de cada país
infla_total <- df_forecast %>% 
  dplyr::select(one_of(lista))

#Transformo a ts
infla_total <- ts(infla_total, start = c(1961,1), frequency = 4)
infla_total <- diff(log(infla_total), lag=4)

# Me quedo con las mismas observaciones que las series completas
infla_total <- tail(infla_total, -1)
infla_total <- head(infla_total, -4)

# Serie del ipc usa
ipc_usa <- df_usa$cpiqus[c(-1,-191,-192,-193,-194)]
ipc_usa <- ts(ipc_usa, start = c(1960,2), frequency = 4)

# Serie del ipq usa
ipq_usa <- df_usa$ipqus[c(-1,-191,-192,-193,-194)]
ipq_usa <- ts(ipq_usa, start = c(1960,2), frequency = 4)

# Serie de m3 usa
m3_usa <- df_usa$m3qus[c(-1,-191,-192,-193,-194)]
m3_usa <- ts(m3_usa, start = c(1960,2), frequency = 4)

# Serie de commodities usa
com <- df_usa$comprice [c(-1,-191,-192,-193,-194)]
com <- ts(com, start = c(1960,2), frequency = 4)

# Creo vector de vectores para las series en diferencias interanuales y le doy configurtacion de ts
series_completas <- cbind(diff(log(ipc_usa), lag = 4),
                          diff(log(ipq_usa), lag = 4),
                          diff(log(com),     lag = 4),
                          diff(log(m3_usa),  lag = 4))

series_completas <- ts(series_completas, start = c(1961,2), frequency = 4)
colnames(series_completas) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2")

#Calculo componentes principales
pr.out <- prcomp(infla_total, scale =TRUE)
# PC  <- scale(infla_total)%*%eigen(cor(infla_total))$vectors
# PC1 <- ts(PC[,1], start = c(1961,1), frequency = 4)

#Verifico si esta bien calcualdo
colSums(pr.out$rotation^2)
zapsmall(cor(pr.out$x))

# Calculo proporcion de la varianza total explicada por cada componente
prop_varianza <- pr.out$sdev^2 / sum(pr.out$sdev^2)

# grafico varianza explicada por cada pc
ggplot(data = data.frame(prop_varianza, pc = 1:22),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")

# Elijo el primer componente que explica casi el 75% de la varianza
PC1 <- pr.out$x[,1]
PC1 <- ts(PC1, start = c(1961,2), frequency = 4)

#Grafico primer componente
plot(PC1, main="Primer componente principal", ylab="Inflación interanual", xlab="Período")

# Junto series y primer componente 
series_favar <- cbind(series_completas, PC1)
colnames(series_favar) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2", "PC1")

# Le doy configuracion de ts
series_favar <- ts(series_favar, start = c(1961,2), frequency = 4)

# Estimo modelo FAVAR
VARselect(series_favar, type = "const")

# Estimo con 2 lags por parsimonia
favar <- VAR(series_favar, lag.max = 2, type = "const")

# Test multivariado (de tipo Portmanteau) de autocorrelación
serial.test(favar, lags.pt = 13) #Rechazo H0, hay autocorrelacion en los errores
# Test de Jarque-Bera de normalidad en los errores
normality.test(favar, multivariate.only = TRUE) # Rechazo H0, no hay normalidad en los errores

# Armo forecast para h=1 rolling out of sample
fcst.favar <- matrix(0, nrow = 25, ncol = 1)  
fcst.favar <- ts(fcst.favar, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_favar, end = 2001 + (i-1)/4)
  favar <- VAR(train, p = 2, type = "const")
  fcst.favar[i,] <- forecast(favar, h=1)$forecast$dlog.ipc$mean
}

# OUT OF SAMPLE: desde 2000Q1 en niveles
out.of.sample <- cbind(log(ipc_usa[161:190]), 
                       log(ipq_usa[161:190]),
                       log(com[161:190]),
                       log(m3_usa[161:190]))

out.of.sample <- ts(out.of.sample, start = c(2000,1), frequency = 4)

# OUT OF SAMPLE: desde 2001Q1 en diferencias
dlogs.test <- diff(out.of.sample, lag = 4)[,1]

# Comparo con el out of sample
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Inflacion", ylab = "", xlab = "")
plot(fcst.favar, col = "red", lwd = 2, main="Forecast FAVAR(2)", ylab = "", xlab = "")
