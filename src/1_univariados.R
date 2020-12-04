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

# Parto la base en train (5/6) y test (1/6)
train <- window(ipc_usa, end = c(2000,4))
test  <- window(ipc_usa, start = c(2001,1))

#Estimamos un ARIMA
fit.arima <- auto.arima(train)
summary(fit.arima)
checkresiduals(fit.arima)

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
