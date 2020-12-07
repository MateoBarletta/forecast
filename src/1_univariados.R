library(dplyr)
library(readxl)
library(forecast)
library(janitor)

# Cargo toda la base y le limpio los nombres a las columnas
df_forecast <- read_excel("data/forecast_DATA.xlsx") %>% 
  clean_names()

# Selecciono solo los datos de USA 
df_usa <- df_forecast %>% 
  select(date, ends_with("us"), comprice)

# Serie del ipc usa
ipc_usa <- ts(df_usa$cpiqus, start = c(1960,1), frequency = 4)
plot.ts(ipc_usa, main = "Indice de Precios al Consumidor de Estados Unidos")

infla_usa <- diff(log(ipc_usa), lag = 4)

# Estimo ARIMA para la serie en niveles
# Parto la base en train (5/6) y test (1/6)
train <- window(ipc_usa, end = c(2000,4))
test  <- window(ipc_usa, start = c(2001,1))

#Estimamos un ARIMA
fit.arima <- auto.arima(train)
summary(fit.arima)
checkresiduals(fit.arima)

#Test de autocorrelacion de los errores:
res <- residuals(fit.arima)
Box.test(res, lag=12, fitdf=3, type="Box-Pierce")
Box.test(res, lag=12, fitdf=3, type="Ljung-Box")

#Estimamos un ETS
fit.ets <- ets(train)
summary(fit.ets)
checkresiduals(fit.ets)

#Generamos los pronósticos:
fcst.arima <- forecast(fit.arima, h = length(test))
fcst.ets   <- forecast(fit.ets, h = length(test))
autoplot(fcst.arima)
autoplot(fcst.ets)

#Evaluamos el desempeño:
accuracy(fcst.arima$mean, test)
accuracy(fcst.ets$mean, test)


# Ahora voy a estimar un ARIMA para la serie en diferencias
infla_usa <- diff(log(ipc_usa), lag = 4)

# DF `de prueba y entrenamiento`
train <- window(infla_usa, end = c(2000,4))
test  <- window(infla_usa, start = c(2001,1))

#Estimamos un ARIMA
fit.arima.diff <- auto.arima(train)
summary(fit.arima.diff)
checkresiduals(fit.arima.diff)

#Generamos los pronósticos rolling para h=1:
fcst.arima <- matrix(0, nrow = 25, ncol = 1)  
fcst.arima <- ts(fcst.var, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(infla_usa, end = 2001 + (i-1)/4)
  arima <- auto.arima(train)
  fcst.arima[i,] <- forecast(arima, h=1)$mean
}

# Comparo con el out of sample
dlogs.test <- diff(out.of.sample, lag = 4)[,1]
par(mfrow=c(1,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,2))
plot(dlogs.test, main="Inflación USA", ylab = "", xlab = "")
plot(fcst.var, col = "grey", lwd = 2, main="Forecast ARIMA", ylab = "", xlab = "")
