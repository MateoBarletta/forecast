# COMPARO MODELOS

# Medidas de error ARIMA
accuracy(fcst.arima, test.dlog.ipc)

# Medidas de error VAR
accuracy(fcst.var, test.dlog.ipc)

# Medidas de error FAVAR
accuracy(fcst.favar, test.dlog.ipc)

# Medidas de error FAR
accuracy(fcst.far, test.dlog.ipc)


ac_arima <- accuracy(fcst.arima, test.dlog.ipc)
ac_var   <- accuracy(fcst.var, test.dlog.ipc)
ac_favar <- accuracy(fcst.favar, test.dlog.ipc)
ac_far   <- accuracy(fcst.far, test.dlog.ipc)

tabla_accuracy <- rbind(ac_arima, ac_var, ac_favar, ac_far)

# Medidas de error VAR
accuracy(fcst.var, test.dlog.ipc)

# Medidas de error FAVAR
accuracy(fcst.favar, test.dlog.ipc)

# Medidas de error FAR
accuracy(fcst.far, test.dlog.ipc)

# El FAVAR minimiza los criterios seguidos (RMSE, MAE y MAPE)
# El modelo ARIMA mejora con respecto al VAR

#Obtengo los errores de pronostico:
e.fcst.arima <- test.dlog.ipc-fcst.arima
e.fcst.var   <- test.dlog.ipc-fcst.var
e.fcst.favar <- test.dlog.ipc-fcst.favar
e.fcst.far   <- test.dlog.ipc-fcst.far

# Test de Diebold-Mariano:
dm.test(e.fcst.arima, e.fcst.var, alternative = "two.sided", power = 2, h=1)
dm.test(e.fcst.arima, e.fcst.favar, alternative = "two.sided", power = 2, h=1)
dm.test(e.fcst.arima, e.fcst.far, alternative = "two.sided", power = 2, h=1)

# No rechazo Diebold-Mariano, los modelos tienen el mismo poder predictivo
