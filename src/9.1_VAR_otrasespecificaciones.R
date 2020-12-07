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
Comparo todos los modelos
accuracy(fcst.var, dlogs.test)
accuracy(fcst.var2, dlogs.test)
accuracy(fcst.var22, dlogs.test)

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
