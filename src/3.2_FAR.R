## MODELO FAR (QUEDA PARA ARMAR
fcst.favar <- matrix(0, nrow = 25, ncol = 1)  
fcst.favar <- ts(fcst.favar, start=c(2001,1), frequency = 4)

for(i in 1:25){
  train <- window(series_favar, end = 2001 + (i-1)/4)
  far <- lm(dlog.ipc ~ lag(dlog.ipc) PC1)
  fcst.favar[i,] <- forecast(favar, h=1)$forecast$dlog.ipc$mean
}