library(dplyr)
library(readxl)
library(forecast)
library(urca)

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



### Test de Dickey Fueller Aumentado: H0)Hay raiz unitaria ##
## IPC ##
# Test de raiz unitaria para la serie en niveles
summary(ur.df(ipc_usa, type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-3.07 vs -3.99)

# Test de raiz unitaria para la serie en logaritmos
summary(ur.df(log(ipc_usa), type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-.77 vs -3.99)

# Test de raiz unitaria para la serie en diferencias
summary(ur.df(diff(log(ipc_usa)), type="drift", selectlags = "BIC"))
# Rechazo H0, el valor del estadistico de t no es menor que el de referencia (-4.2 vs -3.46)

# Test de raiz unitaria para la serie en segundas diferencias
summary(ur.df(diff(log(ipc_usa), differences = 2), type="drift", selectlags = "BIC"))
# Rechazo H0, el valor del estadistico de t no es menor que el de referencia (-18.37 vs -3.46)
## La serie del IPC lleva una o dos diferencias para que sea estacionaria en media


## IPQ ##
# Test de raiz unitaria para la serie en niveles
summary(ur.df(ipq_usa, type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-1.75 vs -3.99)

# Test de raiz unitaria para la serie en logaritmos
summary(ur.df(log(ipq_usa), type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-3.51 vs -3.99)

# Test de raiz unitaria para la serie en diferencias
summary(ur.df(diff(log(ipq_usa)), type="drift", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-3.46 vs -3.46)

# Test de raiz unitaria para la serie en segundas diferencias
summary(ur.df(diff(log(ipq_usa), differences = 2), type="drift", selectlags = "BIC"))
# Rechazo H0, el valor del estadistico de t no es menor que el de referencia (-13.81 vs -3.46)
## La serie del IPQ lleva dos diferencias para que sea estacionaria en media

## M3 ##
# Test de raiz unitaria para la serie en niveles
summary(ur.df(m3_usa, type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (1.56 vs -3.99)

# Test de raiz unitaria para la serie en logaritmos
summary(ur.df(log(m3_usa), type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-0.69 vs -3.99)

# Test de raiz unitaria para la serie en diferencias
summary(ur.df(diff(log(m3_usa)), type="drift", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-5.22 vs -3.46)

# Test de raiz unitaria para la serie en segundas diferencias
summary(ur.df(diff(log(m3_usa), differences = 2), type="drift", selectlags = "BIC"))
# Rechazo H0, el valor del estadistico de t no es menor que el de referencia (-14.12 vs -3.46)
## La serie del M3 lleva una o dos diferencias para que sea estacionaria en media

## COMODITIES ##
# Test de raiz unitaria para la serie en niveles
summary(ur.df(com, type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-1.51 vs -3.99)

# Test de raiz unitaria para la serie en logaritmos
summary(ur.df(log(com), type="trend", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-1.69 vs -3.99)

# Test de raiz unitaria para la serie en diferencias
summary(ur.df(diff(log(com)), type="drift", selectlags = "BIC"))
# No rechazo H0, el valor del estadistico de t no es menor que el de referencia (-6.46 vs -3.46)

# Test de raiz unitaria para la serie en segundas diferencias
summary(ur.df(diff(log(com), differences = 2), type="drift", selectlags = "BIC"))
# Rechazo H0, el valor del estadistico de t no es menor que el de referencia (-15.32 vs -3.46)
## La serie de com lleva una o dos diferencias para que sea estacionaria en media