# COMPARO MODELOS

# Medidas de error ARIMA
accuracy(fcst.arima, dlogs.test)

# Medidas de error VAR
accuracy(fcst.var, dlogs.test)

# Medidas de error FAVAR
accuracy(fcst.favar, dlogs.test)

# El FAVAR minimiza los criterios seguidos (RMSE, MAE y MAPE)
# El modelo ARIMA mejora con respecto al VAR

#Obtengo los errores de pronostico:
e.fcst.arima <- dlogs.test-fcst.arima
e.fcst.var   <- dlogs.test-fcst.var
e.fcst.favar <- dlogs.test-fcst.favar

# Test de Diebold-Mariano:
dm.test(e.fcst.favar, e.fcst.arima, alternative = "two.sided", power = 2, h=1)
dm.test(e.fcst.favar, e.fcst.var, alternative = "two.sided", power = 2, h=1)

# No rechazo Diebold-Mariano, los modelos tienen el mismo poder predictivo
