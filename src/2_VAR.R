library(tidyverse)
library(readxl)
library(forecast)
library(janitor)
library(vars)

# Cargo toda la base y le limpio los nombres a las columnas
df_forecast <- read_excel("data/forecast_DATA.xlsx") %>% 
  clean_names()

# Selecciono solo los datos de USA 
df_usa <- df_forecast %>% 
  dplyr::select(date, ends_with("us"), comprice)

#Proceso las series para sacar valores missing y queden todas de igual magnitud. Asimismo, se transformas a ts
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

colnames(series_completas) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2")

# Grafico de las series en niveles (no grafico commoditites)
df_usa_long <- df_usa %>% 
  pivot_longer(2:4, names_to='indice', values_to='valor')

ggplot(df_usa_long, aes(x=date, y=valor, color=indice))+
  geom_line() +
  ggtitle('Evolución de los indices')

# MODELO VAR
# Elijo el mejor modelo en función de los criterios de información
VARselect(series_completas, type = "const") 

# Estimo modelo
var <- VAR(series_completas, lag.max = 6, type = "const")
# Test multivariado (de tipo Portmanteau) de autocorrelación
serial.test(var, lags.pt = 13) # Rechazo H0, hay autocorrelacion en los errores
# Test de Jarque-Bera de normalidad en los errores
normality.test(var,multivariate.only = TRUE) # Rechazo H0, los errores no son normales

### REALIZAMOS FORECAST
# Partimos la muestra
#in.sample: 2° trimestre 1960 - 4° trimestre 2000 (T=164)
in.sample <- cbind(log(ipc_usa[2:164]), 
                   log(ipq_usa[2:164]),
                   log(com[2:164]),
                   log(m3_usa[2:164]))
in.sample <- ts(in.sample, start = c(1960,2), frequency = 4)

#out.of.sample: 1° trimestre 2000 - 2° trimestre 2007 (T=164) en niveles
out.of.sample <- cbind(log(ipc_usa[161:190]), 
                       log(ipq_usa[161:190]),
                       log(com[161:190]),
                       log(m3_usa[161:190]))
out.of.sample <- ts(out.of.sample, start = c(2000,1), frequency = 4)

## MODELO VAR (en diferencias)
# Estimo un VAR en diferencias para el periodo in-sample
# Diferencio la serie in sample
din.sample <- diff(in.sample, lag = 4)

# Veo que especificacion minimiza los criterios de informacion: 3 lags
VARselect(din.sample, lag.max = 13, type = "const")

# Estimo el modelo VAR(6)
fit.var <- VAR(din.sample, p = 6, type = "const")
serial.test(fit.var, lags.pt = 13) # No rechazo no autocorrelacion en los errores.

# Actualizamos recursivamente los parametros del VAR(3)
#Pronostico para h=1 la difencia logaritmica de serie IPC (inflacion) VAR1
fcst.var <- matrix(0, nrow = 25, ncol = 1)  
fcst.var <- ts(fcst.var, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_completas, end = 2001 + (i-1)/4)
  var <- VAR(train, p = 6, type = "const")
  fcst.var[i,] <- forecast(var, h=1)$forecast$dlog.ipc$mean
}

# Comparo con el out of sample
test.dlog.ipc <- diff(out.of.sample, lag = 4)[,1]
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(test.dlog.ipc , main="Inflación USA", xlab = "Período", ylab = "IPC USA", ylim=c(0, 0.040))
plot(fcst.var, col = "blue", lwd = 2, main="Forecast VAR(6)", xlab = "Período", ylab = "IPC USA", ylim=c(0, 0.040))

rm(i, train, var)
