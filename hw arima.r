# Dependencias para datos
library("lubridate")
library("forecast")
library("tseries")

db <- readxl::read_excel("Datos\\indicadores2020.xls", sheet = 1)

#Convertir columna periodos en date
db$periodos <- ym(db$periodos)

## convertimos db en time series
db_ts <- ts(db$datos, start = c(1993, 1), frequency = 12)

## Para suavizar la serie y estabilizar la varianza aplicamos logaritmos
db_log <- ts(log(db$datos), start = c(1993, 1), frequency = 12)

## Para eliminar tendencia, utilizamos primeras diferencias
first_difference <- diff(db_log)

## Eliminar estacionalidad, diferencias estacionales
seasonal_difference <- diff(first_difference, lag = 12)

#Holt Winter model
n_training <- 324
n_test <- 13
training_set_hw <- head(db_ts, n_training)
test_set_hw <- tail(db_ts, n_test)

hw_forecast <- hw(training_set_hw, seasonal = "multiplicative")

hw_forecast <- forecast(hw_forecast, h = 13)

autoplot(hw_forecast) + autolayer(test_set_hw)

#ARIMA model
arima_forecast <- arima(training_set_hw, order = c(2, 1, 2))

arima_forecast <- forecast(arima_forecast, h = 13)
autoplot(arima_forecast) + autolayer(test_set_hw)

#Diebold-Mariano test
dm.test(hw_forecast$residuals,
    arima_forecast$residuals,
    h = 13,
    alternative = "less")