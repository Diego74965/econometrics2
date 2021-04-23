# Dependencias para graficas y formato
library("ggplot2")
library("ggfortify")
library("cowplot")
library("ggseas")
library("extrafont")
library("grid")
library("scales")
library("gt")
library("ggthemes")
source("script.r")

# Dependencias para datos
library("lubridate")
library("seasonal")
library("aTSA")
library("tseries")
library("forecast")

# importar fonts (usar una vez)
font_import()
loadfonts(device = "win")
windowsFonts()

db <- readxl::read_excel("Datos\\indicadores2020.xls", sheet = 1)

### leyendas de las graficas
fuente <- function(x, y) {
    grid.text("Fuente: Elaboración propia con datos del INEGI", x, y,
        gp = gpar(fontfamily = "Arial", fontface = "italic", cex = 0.7))
}

#Convertir columna periodos en date
db$periodos <- ym(db$periodos)

# Serie original
ggplot(data = db, mapping = aes(x = periodos, y = datos)) +
    geom_line() + theme_cowplot(12, "Times New Roman") +
    labs(x = "Año", y = "IMAI") +
    theme(plot.title = element_text(hjust = 0.5) + theme.fxdat)
fuente(0.8, 0.01)

# Ajustar serie

## convertimos db en time series
db_ts <- ts(db$datos, start = c(1993, 1), frequency = 12)

plot(decompose(db_ts),
    xlab = "Año",
    cex.lab = 0.58,
    cex.axis = 0.92,
    main = NULL)

#######ggseas (misma grafica pero en ggplot)
ggsdc(data = db, aes(x = periodos, y = datos),
    method = "seas", start = c(1993, 1), frequency = 12) +
    geom_line() +
    labs(x = "Año", colour = "", y = "  \n") +
    theme_cowplot(12, "Times New Roman") +
    theme(legend.position = c(0.17, 0.92))
fuente(0.8, 0.03)

## Para suavizar la serie y estabilizar la varianza aplicamos logaritmos
db_log <- ts(log(db$datos), start = c(1993, 1), frequency = 12)

db_log_plot <- autoplot(db_log) +
    labs(y = "log(datos)", x = "Año") +
    theme_cowplot(12, "Times New Roman")

db_plot <- autoplot(db_ts) +
    labs(y = "datos", x = "Año") +
    theme_cowplot(12, "Times New Roman")

### serie log
autoplot(db_log) +
    labs(y = "log(datos)", x = "Año", title = "Transformación logarítmica") +
    theme_cowplot(12, "Times New Roman") +
    theme(plot.title = element_text(hjust = 0.5))
fuente(0.8, 0.01)

#### Serie original y log
plot_grid(db_plot, db_log_plot, ncol = 1, align = "v")
fuente(0.8, 0.01)

## Para eliminar tendencia, utilizamos primeras diferencias
first_difference <- diff(db_log)
autoplot(first_difference) + theme_cowplot(12, "Times New Roman") +
    labs(x = "Año", y = "Dlog(db_ts)", title = "Primeras diferencias") +
    theme(plot.title = element_text(hjust = 0.5))
fuente(0.8, 0.01)

## Eliminar estacionalidad, diferencias estacionales
seasonal_difference <- diff(first_difference, lag = 12)

autoplot(seasonal_difference) +
    labs(x = "Año", y = "IMAI ajustado",
    title = "Diferencias estacionales") +
    theme_cowplot(12, "Times New Roman") +
    theme(plot.title = element_text(hjust = 0.5))
fuente(0.8, 0.01)

comparassion <- cbind(seasonal_difference, first_difference)
autoplot(comparassion, facets = FALSE, xlab = "Año", ylab = "IMAI")

plot(seasonal_difference)

## Pruebas de estacionariedad
### Dickey fuller aumentada ADF

#### Testeando serie original
adf.test(db_ts, k = 12)# p-value > .05, no rechazas H0, no es estacionaria
pp.test(db_ts)  # p-value < .05, rechazas H0, es estacionaria?
kpss.test(db_ts) # p-value < .05, rechazas H0 no es estacionaria
plot(db_ts)

#### Testeando serie transformada
adf.test(seasonal_difference, k = 12)#p-value < .05, rechazas H0 es estacionaria
pp.test(seasonal_difference) # p-value < .05, rechazas H0, es estacionaria
kpss.test(seasonal_difference) # p-value > .05, no rechazas H0, es estacionaria

# Graficar ambas
db_sd_plot <- autoplot(seasonal_difference) +
    labs(y = "IMAI ajustado", x = "Año") +
    theme_cowplot(12, "Times New Roman")

db_ts_plot <- autoplot(db_ts) +
    labs(y = "IMAI", x = "Año") +
    theme_cowplot(12, "Times New Roman")

plot_grid(db_ts_plot, db_sd_plot, ncol = 1, align = "v")
fuente(0.8, 0.02)


## FORECAST SIMPLE MODEL ######
n_training <- ((2020 - 1994) * 12) - 1 #Datos de 1993 a 2019
n_test <- 13 #Datos de enero 2020 a enero 2021

training_set <- head(seasonal_difference, n_training)
test_set <- tail(seasonal_difference, n_test)

### Mean forecast
mean_forecast <- meanf(training_set, h = 13, level = c(80, 95, 99))

plot_fx(mean_forecast, date.breaks = "3 years", date.format = "%Y",
    x.title = "Año")

accuracy(mean_forecast, test_set)

### Random walk (= naive)
naive_forecast <- naive(training_set, h = 13, level = c(80, 95, 99))

plot_fx(naive_forecast, date.breaks = "3 years", date.format = "%Y",
    x.title = "Año")

accuracy(naive_forecast, test_set)

## Random walk estacional
snaive_forecast <- snaive(training_set, h = 13, level = c(80, 95, 99))

plot_fx(snaive_forecast, date.breaks = "3 years", date.format = "%Y",
    x.title = "Año")

accuracy(snaive_forecast, test_set)

### Comparar modelos con test set

## Modelo de Pronosticos
autoplot(mean_forecast) +
    labs(y = "IMAI", x = "Año", title = NULL) +
    autolayer(test_set) +
    theme_cowplot(12, "Times New Roman") +
    theme(legend.position = "none")

## Modelo naive
autoplot(naive_forecast) +
    labs(y = "IMAI", x = "Año", title = NULL) +
    autolayer(test_set) +
    theme_cowplot(12, "Times New Roman") +
    theme(legend.position = "none")

## Modelo snaive
autoplot(snaive_forecast) +
    autolayer(test_set) +
    labs(y = "IMAI", x = "Año", title = NULL) +
    theme_cowplot(12, "Times New Roman") +
    theme(legend.position = "none")