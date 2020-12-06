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

series_completas <- cbind(diff(log(ipc_usa), lag = 4),
                          diff(log(ipq_usa), lag = 4),
                          diff(log(com),     lag = 4),
                          diff(log(m3_usa),  lag = 4))

colnames(series_completas) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2")

#LAS SERIES DE IPC Y M3 hay que diferenciarlas dos veces para estacionalizarlas, hay que incluirlas asi en el VAR?

#hago log y diferencio para cada serie 
# pba <- df_usa %>% 
#   filter(!is.na(m3qus)) %>% 
#   mutate(dlog_ipc = c(NA, diff(log(cpiqus))),
#          dlog_ipq = c(NA, diff(log(ipqus))),
#          dlog_m3  = c(NA, diff(log(m3qus))),
#          dlog_com = c(NA, diff(log(comprice))))

# Grafico de las series en niveles (no grafico commoditites)
df_usa_long <- df_usa %>% 
  pivot_longer(2:4, names_to='indice', values_to='valor')

ggplot(df_usa_long, aes(x=date, y=valor, color=indice))+
  geom_line() +
  ggtitle('Evolución de los indices')

# cbind(diff(log(ipc_usa)), diff(log(ipq_usa)),diff(log(com)),diff(log(m3_usa)))

# Elijo el mejor modelo en función de los criterios de información
VARselect(series_completas, type = "const") 

# Estimo modelo
var1 <- VAR(series_completas, lag.max = 9, type = "const")
# Test multivariado (de tipo Portmanteau) de autocorrelación
serial.test(var1, lags.pt = 13) # Rechazo H0, hay autocorrelacion en los errores
# Test de Jarque-Bera de normalidad en los errores
normality.test(var1,multivariate.only = TRUE) # Rechazo H0, los errores no son normales

# Pruebo con el VAR en diferencias pero no interanuales, sino en niveles (la serie no parece tener estacionalidad)
#MB: Pruebo de hacer el VAR para las series con una diferencia.
series_completas2 <- cbind(
  diff(log(ipc_usa)),
  diff(log(ipq_usa)),
  diff(log(m3_usa)),
  diff(log(com))
)

colnames(series_completas2) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2")
VARselect(series_completas2, type = "const") 

# Estimo modelo
var2 <- VAR(series_completas2, lag.max = 3, type = "const")
# Test multivariado (de tipo Portmanteau) de autocorrelación
serial.test(var2, lags.pt = 13) # No rechazo H0, no hay autocorrelacion en los errores
# Test de Jarque-Bera de normalidad en los errores
normality.test(var2, multivariate.only = TRUE) # Rechazo H0, no hay normalidad en los errores


#MB: Pruebo de hacer el VAR para las series dos veces diferenciadas.
series_completas22 <- cbind(
  diff(log(ipc_usa), differences=2),
  diff(log(ipq_usa), differences=2),
  diff(log(m3_usa),  differences=2),
  diff(log(com),     differences=2)
)

colnames(series_completas22) <- c("dlog.ipc", "dlog.ipq","dlog.como","dlog.m2")

VARselect(series_completas22, type = "const") 

##MB:Bajo las dos especificaciones me sugiere un AR(3), lo que podría estar en lines con el MA(3) que nos dio en la primera parte.

# Estimo modelo
var22 <- VAR(series_completas22, lag.max = 3, type = "const")
# Test multivariado (de tipo Portmanteau) de autocorrelación
serial.test(var22, lags.pt = 13) # No rechazo H0, no hay autocorrelacion en los errores
# Test de Jarque-Bera de normalidad en los errores
normality.test(var2, multivariate.only = TRUE) # Rechazo H0, no hay normalidad en los errores


### REALIZAMOS FORECAST
# Partimos la muestra
#in.sample: 2° trimestre 1960 - 4° trimestre 2000 (T=164)
in.sample <- cbind(log(ipc_usa[2:164]), 
                   log(ipq_usa[2:164]),
                   log(com[2:164]),
                   log(m3_usa[2:164]))
in.sample <- ts(in.sample, start = c(1960,2), frequency = 4)

#out.of.sample: 1° trimestre 2000 - 2° trimestre 2007 (T=164)
out.of.sample <- cbind(log(ipc_usa[165:190]), 
                       log(ipq_usa[165:190]),
                       log(com[165:190]),
                       log(m3_usa[165:190]))
out.of.sample <- ts(out.of.sample, start = c(2001,1), frequency = 4)

## MODELO VAR (en diferencias)
# Estimo un VAR en diferencias para el periodo in-sample
# Diferencio la serie in sample
din.sample <- diff(in.sample)

# Veo que especificacion minimiza los criterios de informacion: 3 lags
VARselect(din.sample, lag.max = 13, type = "const")

# Estimo el modelo
in_var1 <- VAR(din.sample, p = 3, type = "const")
summary(in_var1)
serial.test(in_var1, lags.pt = 13) # No rechazo no autocorrelacion en los errores.

# Actualizamos recursivamente los parametros del VAR(3)
#Pronostico para h=1 la difencia logaritmica de serie IPC (inflacion) VAR1
fcst.var <- matrix(0, nrow = 25, ncol = 1)  
fcst.var <- ts(fcst.var, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_completas, end = 2001 + (i-1)/4)
  var <- VAR(train, p = 3, type = "const")
  fcst.var[i,] <- forecast(var, h=1)$forecast$dlog.ipc$mean
}

# Comparo con el out of sample
dlogs.test <- diff(out.of.sample)[,1]
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Inflacion", ylab = "", xlab = "")
plot(fcst.var, col = "grey", lwd = 2, main="Forecast VAR(3)", ylab = "", xlab = "")


#Pronostico para h=1 la difencia logaritmica de serie IPC (inflacion) VAR2
fcst.var2 <- matrix(0, nrow = 25, ncol = 1)  
fcst.var2 <- ts(fcst.var2, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_completas2, end = 2001 + (i-1)/4)
  var <- VAR(train, p = 3, type = "const")
  fcst.var2[i,] <- forecast(var, h=1)$forecast$dlog.ipc$mean
}

# Comparo con el out of sample
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Inflacion", ylab = "", xlab = "")
plot(fcst.var2, col = "blue", lwd = 2, main="Forecast VAR2(3)", ylab = "", xlab = "")

#Pronostico para h=1 la difencia logaritmica de serie IPC (inflacion) VAR22
fcst.var22 <- matrix(0, nrow = 25, ncol = 1)  
fcst.var22 <- ts(fcst.var22, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_completas22, end = 2001 + (i-1)/4)
  var <- VAR(train, p = 3, type = "const")
  fcst.var22[i,] <- forecast(var, h=1)$forecast$dlog.ipc$mean
}

# Comparo con el out of sample
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Inflacion", ylab = "", xlab = "")
plot(fcst.var2, col = "red", lwd = 2, main="Forecast VAR22(3)", ylab = "", xlab = "")

# Comparo todos los modelos
accuracy(fcst.var, dlogs.test)
accuracy(fcst.var2, dlogs.test)
accuracy(fcst.var22, dlogs.test)


# El que mejor performance tiene es el VAR2