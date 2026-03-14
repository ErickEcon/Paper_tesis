#install.packages(c("quantmod","rugarch","rmgarch"))   # only needed in case you have not yet installed these packages
#install.packages(c("tidyr","readxl","tseries","ggplot2","tidyr","MTS"))


rm(list=ls())

library(readxl)
library(quantmod)
library(rugarch)
library(rmgarch)
library(tseries)
library(ggplot2)
library(tidyr)
library(MTS)
library(zoo)


#setwd("D:/Python/Eviews/R")

Ret <- read_excel("D:/Python/Eviews/Inputs/Retornos.xlsx")

Retornos <- Ret[,-1] * 100

#############################################################
# Ratio Sharpe Pre-estimación
#############################################################
fechas <- Ret$Fecha
# Todo el periodo
##########################

# 1. Definir Rf (asegúrate de tener este valor, ej: 2% anual)
rf_anual <- 0.02 
rf_diario <- rf_anual / 252

# 2. Calcular Sharpe Anualizado Correcto
# Fórmula: (Retorno Medio Diario - Rf Diario) / Desv. Std Diaria * Raiz(252)

sharpe_final_tesis <- (colMeans(Ret1) - rf_diario) / apply(Ret1, 2, sd) * sqrt(252)

# 3. Ver resultados e imprimir
print(round(sharpe_final_tesis, 4))

# Por año
#############################

# Sintaxis: xts(Datos_Numericos, order.by = Columna_Fecha)
# Ret[, -1] significa "Todas las columnas MENOS la 1" (porque la 1 es la fecha)
Ret_xts <- xts(Ret[, -1], order.by = fechas)

# 3. Verificar que ya no hay columna de fecha, sino un índice
head(Ret_xts)
# Deberías ver la fecha a la izquierda (en gris) y solo columnas de números.


# ==============================================================================
# DATOS DE EXCEL (Rf Anuales)
# ==============================================================================
mis_tasas_rf <- c(
  "2014" = 0.0254, "2015" = 0.0214, "2016" = 0.0184, "2017" = 0.0233,
  "2018" = 0.0291, "2019" = 0.0214, "2020" = 0.0089, "2021" = 0.0145,
  "2022" = 0.0295, "2023" = 0.0396, "2024" = 0.0420
)

# ==============================================================================
# CÁLCULO SEGURO CON EL OBJETO XTS
# ==============================================================================

# 1. Sacamos los años del índice del nuevo objeto xts
anos_unicos <- unique(format(index(Ret_xts), "%Y"))

# 2. Matriz vacía para resultados
matriz_resultados <- matrix(NA, nrow = length(anos_unicos), ncol = ncol(Ret_xts))
rownames(matriz_resultados) <- anos_unicos
colnames(matriz_resultados) <- colnames(Ret_xts)

# 3. Bucle
for(i in 1:length(anos_unicos)) {
  
  el_ano <- anos_unicos[i]
  
  # AHORA SÍ FUNCIONA: xts entiende que ["2014"] es un filtro de tiempo
  datos_ano <- Ret_xts[el_ano]
  
  # Rf
  rf_actual <- mis_tasas_rf[el_ano]
  if(is.na(rf_actual)) rf_actual <- 0.02 
  
  # Cálculos numéricos
  datos_numericos <- coredata(datos_ano) # Quitamos la fecha para operar
  
  ret_anual <- colMeans(datos_numericos, na.rm = TRUE) * 252
  vol_anual <- apply(datos_numericos, 2, sd, na.rm = TRUE) * sqrt(252)
  
  sharpe_vector <- (ret_anual - rf_actual) / vol_anual
  
  matriz_resultados[i, ] <- as.numeric(sharpe_vector)
}

# 4. Resultado Final
print(round(matriz_resultados, 4))





####################################################
# BVL_r
####################################################
spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 12), include.mean = FALSE),
  distribution.model = "sged",
  fixed.pars = list(
    ma1 = 0,
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ma5 = 0,
    ma6 = 0,
    ma7 = 0,
    ma8 = 0,
    ma9 = 0,
    ma10 = 0,
    ma11 = 0
  )
)


fit_alt <- ugarchfit(spec_alt, data = Retornos$BVL_r)
fit_alt

# QQ Plot
plot(fit_alt, which = 9)

# Plots

ug_sigma <- fit_alt@fit$sigma
ug_var <- fit_alt@fit$var # Estimated conditional variance
ug_res2 <- (fit_alt@fit$residuals)^2 # Residuos al cuadrado



plot(ug_res2, type='l')
lines(ug_var, col='green')


df_plot <- data.frame(
  Fecha = Ret$Fecha,
  Retornos = as.numeric(Retornos$BVL_r), # Asegúrate de usar tus datos reales aquí
  Volatilidad = ug_sigma
)

df_plot

# Forecast
ugfore <- ugarchforecast(fit_alt, n.ahead = 30)
ugfore

ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type='l')

####################################################
# CLX_r
####################################################
acf(Retornos$CLX_r)
pacf(Retornos$CLX_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
  distribution.model = "std"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$CLX_r)
fit_alt


# QQ Plot
plot(fit_alt, which = 9)


residual2 <- fit_alt@fit$residuals^2

acf(residual2)


####################################################
# COLCAP_r
####################################################
acf(Retornos$COLCAP_r)
pacf(Retornos$COLCAP_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,5), include.mean = FALSE),
  distribution.model = "sstd",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$COLCAP_r)
fit_alt

# QQ Plot
plot(fit_alt, which = 9)


residual2 <- fit_alt@fit$residuals^2
acf(fit_alt@fit$residuals)

####################################################
# IPC_Mexico_r
####################################################
acf(Retornos$IPC_Mexico_r)
pacf(Retornos$IPC_Mexico_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(1,1), include.mean = FALSE),
  distribution.model = "sstd"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$IPC_Mexico_r)
fit_alt

# QQ Plot
plot(fit_alt, which = 9)

####################################################
# SP500_r
####################################################
acf(Retornos$SP500_r)
pacf(Retornos$SP500_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "nig",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ar2 = 0,
    ar3 = 0,
    ar4 = 0
  )
)
fit_alt <- ugarchfit(spec_alt, data = Retornos$SP500_r)
fit_alt

# QQ Plot
plot(fit_alt, which = 9)

####################################################
# VIX_r
####################################################
acf(Retornos$VIX_r)
pacf(Retornos$VIX_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,5), include.mean = FALSE),
  distribution.model = "ghyp",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$VIX_r)
fit_alt

# QQ Plot
plot(fit_alt, which = 9)

help('ugarchspec')

####################################################
# TC_Peru
####################################################
acf(Retornos$TC_Peru_r)
pacf(Retornos$TC_Peru_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$TC_Peru_r)
fit_alt

help("ugarchspec")
# QQ Plot
plot(fit_alt, which = 9)

####################################################
# TC_Chile
####################################################
acf(Retornos$TC_Chile_r)
pacf(Retornos$TC_Chile_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$TC_Chile_r)
fit_alt

help("ugarchspec")
# QQ Plot
plot(fit_alt, which = 9)

####################################################
# TC_Colombia
####################################################
acf(Retornos$TC_Colombia_r)
pacf(Retornos$TC_Colombia_r)


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(0,1), include.mean = FALSE),
  distribution.model = "sstd"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos$TC_Colombia_r)
fit_alt

help("ugarchspec")
# QQ Plot
plot(fit_alt, which = 9)

####################################################
# TC_Mexico
####################################################
acf(Retornos$TC_Mexico_r)
pacf(Retornos$TC_Mexico_r)

mexico <- xts(Retornos$TC_Mexico_r, order.by = Ret$Fecha)
plot(mexico, type='l', )
Retornos_Trim <- mexico["2020-07-01/"]


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)

fit_alt <- ugarchfit(spec_alt, data = Retornos_Trim)
fit_alt

help("ugarchspec")
# QQ Plot
plot(fit_alt, which = 9)


####################################################
# Multi BVL_r
####################################################
multi_data <- Retornos[c("BVL_r","TC_Peru_r","SP500_r","VIX_r")]


spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 12), include.mean = FALSE),
  distribution.model = "sged",
  fixed.pars = list(
    ma1 = 0,
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ma5 = 0,
    ma6 = 0,
    ma7 = 0,
    ma8 = 0,
    ma9 = 0,
    ma10 = 0,
    ma11 = 0
  )
)
spec_ot1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)
spec_ot2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "nig",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ar2 = 0,
    ar3 = 0,
    ar4 = 0
  )
)
spec_ot3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,5), include.mean = FALSE),
  distribution.model = "ghyp",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)

uspec.n = multispec(c(spec_alt, spec_ot1, spec_ot2, spec_ot3))


multf = multifit(uspec.n, multi_data)
multf

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution = "mvt")
fit1 = dccfit(spec1, data= multi_data, fit.control = list(eval.se=TRUE), fit=multf)
fit1

####### Pruebas estadísticas de Robustez del modelo ###############

resid_raw <- residuals(fit1)
sigma_cond <- sigma(fit1)
resid_std <- resid_raw / sigma_cond
resid_std <- as.matrix(resid_std)

# Ljung-Box test a los residuos estandarizados
lb_test <- apply(resid_std, 2, function(x) Box.test(x, lag = 10, type = "Ljung-Box"))
sapply(lb_test, function(x) x$p.value)

# Ljung-Box test a los residuos estandarizados al cuadrado
lb_sq_test <- apply(resid_std^2, 2, function(x) Box.test(x, lag = 10, type = "Ljung-Box"))
sapply(lb_sq_test, function(x) x$p.value)


# pruebas de Hosking o Li-McLeod - Residuos
mq_test_levels <- mq(resid_std, lag = 10, adj = 2)


# pruebas de Hosking o Li-McLeod - Residuos al cuadrado
mq_test_squared <- mq(resid_std^2, lag = 10, adj = 2)



######################################################################
cov1 = rcov(fit1) # Extracts the covariance matrix
cor1 = rcor(fit1) # Extracts the correlation matrix

dim(cor1) # Nrow | Ncol | Nobs

cor1[,,dim(cor1)[3]] 


cor_tc <- cor1[2,1,] #Correlación entre la segunda fila y primera columna
cor_tc <- xts(cor_tc, order.by = Ret$Fecha)
# Plots
plot(cor_tc) # Correlación dinámica

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,], order.by = Ret$Fecha),main="BVL_Perú and TC")
plot(as.xts(cor1[1,3,], order.by = Ret$Fecha),main="BVL_Perú and SP500")
plot(as.xts(cor1[1,4,], order.by = Ret$Fecha),main="BVL_Perú and VIX")


#Plot GARCH por cada serie
lista_vols <- lapply(multf@fit, sigma)
vols_combinadas <- do.call(cbind, lista_vols)
colnames(vols_combinadas) <- c("BVL_Peru", "TC_Peru", "SP500_r", "VIX_r")
vols_combinadas <- xts(vols_combinadas, order.by = Ret$Fecha)

df_plot <- data.frame(
  Fecha = index(vols_combinadas),
  coredata(vols_combinadas)
)
df_largo <- pivot_longer(df_plot, 
                         cols = -Fecha, 
                         names_to = "Serie", 
                         values_to = "Volatilidad")

ggplot(df_largo, aes(x = Fecha, y = Volatilidad, color = Serie)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Serie, ncol = 1, scales = "free_y")
  labs(title = "Volatilidad Condicional (GARCH) de las 4 Series",
       x = "Tiempo",
       y = "Volatilidad (Sigma_t)") +
  theme_bw() +
  theme(legend.position = "bottom")

####################################################
# Construcción de naive indicators
####################################################
dcc_cor <- rcor(fit1)
fechas <- Ret$Fecha
nombres <- colnames(multi_data)
  
# Configurar la ventana gráfica para tener 3 gráficos apilados (Verticalmente)
par(mfrow = c(3, 1), mar = c(2, 4, 2, 1)) # Ajusta márgenes
  
# Bucle para comparar Activo 1 contra Activo 2, 3 y 4
for (j in 2:4) {
    
  # --- A. Extraer DCC rho para el par (1, j) ---
  rho_dcc <- xts(dcc_cor[1, j, ], order.by = fechas)
    
  # --- B. Calcular Rolling Correlation (Naive) para el par (1, j) ---
  # Ventana de 90 días (ajusta si prefieres 60 o 120)
  rho_roll <- rollapply(multi_data, width = 90, 
                          function(x) cor(x[,1], x[,j]), 
                          by.column = FALSE, align = "right")
  rho_roll_zoo <- zoo(as.numeric(rho_roll), order.by = fechas) 
  rho_dcc_zoo  <- zoo(as.numeric(rho_dcc), order.by = fechas)
  # --- C. Graficar ---
  # Rango del eje Y entre -1 y 1 para ver bien la correlación
  plot(rho_roll_zoo, type = "l", col = "darkgray", lwd = 3, ylim = c(-1, 1),
       main = paste("Correlación Dinámica:", nombres[1], "vs", nombres[j]),
       ylab = "Correlación", xlab = "Año")
    
  # Añadir la línea del DCC encima (en azul o rojo)
  lines(rho_dcc_zoo, col = "blue", lwd = 1.5)
    
  # Añadir una línea horizontal en 0 como referencia
  abline(h = 0, lty = 2, col = "black")
    
  # Leyenda (solo en el primer gráfico para no ensuciar)
  if (j == 2) {
    legend("bottomright", legend = c("Rolling (90 días)", "DCC-GARCH"),
            col = c("darkgray", "blue"), lwd = c(1.5, 2), bty = "n", cex = 0.8)
  }
}
  
# Restaurar configuración gráfica
par(mfrow = c(1, 1))

#################### Descriptivos de los resultados #######################
dcc_cor <- rcor(fit1)
nombres <- colnames(multi_data) # Para usar los nombres correctos de tus series

# 2. Crear un data.frame solo con los pares que te interesan
# Asumiendo que la Columna 1 es BVL (tu activo principal)
df_correlaciones <- data.frame(
  "BVL_vs_TC"    = dcc_cor[1, 2, ],
  "BVL_vs_SP500" = dcc_cor[1, 3, ],
  "BVL_vs_VIX"   = dcc_cor[1, 4, ]
)

# 3. Definir función para calcular los estadísticos que pide una tesis
# Puedes agregar Skewness o Kurtosis si usas el paquete 'moments', 
# pero estos 5 son los esenciales.
calcular_stats <- function(x) {
  c(
    Media     = mean(x),
    Mediana   = median(x),
    Maximo    = max(x),
    Minimo    = min(x),
    Desv.Std  = sd(x)
  )
}

# 4. Generar la tabla
# 'sapply' aplica la función a cada columna
# 't()' transpone la tabla para que las series queden en filas (más legible)
tabla_resumen <- t(sapply(df_correlaciones, calcular_stats))

# 5. Mostrar la tabla redondeada a 4 decimales
print(round(tabla_resumen, 4))


####################################################
# Multi CLX_r
####################################################
multi_data <- Retornos[,c("CLX_r","TC_Chile_r","SP500_r","VIX_r")]

spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(2, 0), include.mean = FALSE),
  distribution.model = "std"
)


spec_ot <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "std"
)
spec_ot2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "nig",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ar2 = 0,
    ar3 = 0,
    ar4 = 0
  )
)
spec_ot3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,5), include.mean = FALSE),
  distribution.model = "ghyp",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)


uspec.n = multispec(c(spec_alt, spec_ot, spec_ot2, spec_ot3))


multf = multifit(uspec.n, multi_data)
multf

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution = "mvt")
fit1 = dccfit(spec1, data= multi_data, fit.control = list(eval.se=TRUE), fit=multf)
fit1

cov1 = rcov(fit1) # Extracts the covariance matrix
cor1 = rcor(fit1) # Extracts the correlation matrix

dim(cor1) # Nrow | Ncol | Nobs

cor1[,,dim(cor1)[3]] 


cor_tc <- cor1[2,1,] #Correlación entre la segunda fila y primera columna
cor_tc <- xts(cor_tc, order.by = Ret$Fecha)
# Plots
plot(cor_tc) # Correlación dinámica

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,], order.by = Ret$Fecha),main="CLX_Chile and TC")
plot(as.xts(cor1[1,3,], order.by = Ret$Fecha),main="CLX_Chile and SP500")
plot(as.xts(cor1[1,4,], order.by = Ret$Fecha),main="CLX_Chile and VIX")


#GARCH por cada serie

lista_vols <- lapply(multf@fit, sigma)
vols_combinadas <- do.call(cbind, lista_vols)
colnames(vols_combinadas) <- c("CLX(Chile)", "TC(Chile)", "SP500", "VIX")
vols_combinadas <- xts(vols_combinadas, order.by = Ret$Fecha)

df_plot <- data.frame(
  Fecha = index(vols_combinadas),
  coredata(vols_combinadas)
)
df_largo <- pivot_longer(df_plot, 
                         cols = -Fecha, 
                         names_to = "Serie", 
                         values_to = "Volatilidad")

ggplot(df_largo, aes(x = Fecha, y = Volatilidad, color = Serie)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Serie, ncol = 1, scales = "free_y")
labs(title = "Volatilidad Condicional (GARCH) de las 4 Series",
     x = "Tiempo",
     y = "Volatilidad (Sigma_t)") +
  theme_bw() +
  theme(legend.position = "bottom")

####################################################
# Construcción de naive indicators
####################################################
dcc_cor <- rcor(fit1)
fechas <- Ret$Fecha
nombres <- colnames(multi_data)

# Configurar la ventana gráfica para tener 3 gráficos apilados (Verticalmente)
par(mfrow = c(3, 1), mar = c(2, 4, 2, 1)) # Ajusta márgenes

# Bucle para comparar Activo 1 contra Activo 2, 3 y 4
for (j in 2:4) {
  
  # --- A. Extraer DCC rho para el par (1, j) ---
  rho_dcc <- xts(dcc_cor[1, j, ], order.by = fechas)
  
  # --- B. Calcular Rolling Correlation (Naive) para el par (1, j) ---
  # Ventana de 90 días (ajusta si prefieres 60 o 120)
  rho_roll <- rollapply(multi_data, width = 90, 
                        function(x) cor(x[,1], x[,j]), 
                        by.column = FALSE, align = "right")
  rho_roll_zoo <- zoo(as.numeric(rho_roll), order.by = fechas) 
  rho_dcc_zoo  <- zoo(as.numeric(rho_dcc), order.by = fechas)
  # --- C. Graficar ---
  # Rango del eje Y entre -1 y 1 para ver bien la correlación
  plot(rho_roll_zoo, type = "l", col = "darkgray", lwd = 3, ylim = c(-1, 1),
       main = paste("Correlación Dinámica:", nombres[1], "vs", nombres[j]),
       ylab = "Correlación", xlab = "Año")
  
  # Añadir la línea del DCC encima (en azul o rojo)
  lines(rho_dcc_zoo, col = "blue", lwd = 1.5)
  
  # Añadir una línea horizontal en 0 como referencia
  abline(h = 0, lty = 2, col = "black")
  
  # Leyenda (solo en el primer gráfico para no ensuciar)
  if (j == 2) {
    legend("bottomright", legend = c("Rolling (90 días)", "DCC-GARCH"),
           col = c("darkgray", "blue"), lwd = c(1.5, 2), bty = "n", cex = 0.8)
  }
}

# Restaurar configuración gráfica
par(mfrow = c(1, 1))

#################### Descriptivos de los resultados #######################
dcc_cor <- rcor(fit1)
nombres <- colnames(multi_data) # Para usar los nombres correctos de tus series

# 2. Crear un data.frame solo con los pares que te interesan
# Asumiendo que la Columna 1 es BVL (tu activo principal)
df_correlaciones <- data.frame(
  "BVL_vs_TC"    = dcc_cor[1, 2, ],
  "BVL_vs_SP500" = dcc_cor[1, 3, ],
  "BVL_vs_VIX"   = dcc_cor[1, 4, ]
)

# 3. Definir función para calcular los estadísticos que pide una tesis
# Puedes agregar Skewness o Kurtosis si usas el paquete 'moments', 
# pero estos 5 son los esenciales.
calcular_stats <- function(x) {
  c(
    Media     = mean(x),
    Mediana   = median(x),
    Maximo    = max(x),
    Minimo    = min(x),
    Desv.Std  = sd(x)
  )
}

# 4. Generar la tabla
# 'sapply' aplica la función a cada columna
# 't()' transpone la tabla para que las series queden en filas (más legible)
tabla_resumen <- t(sapply(df_correlaciones, calcular_stats))

# 5. Mostrar la tabla redondeada a 4 decimales
print(round(tabla_resumen, 4))



####################################################
# Multi COLCAP_r
####################################################
multi_data <- Retornos[,c("COLCAP_r","TC_Colombia_r","SP500_r","VIX_r")]

spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,5), include.mean = FALSE),
  distribution.model = "sstd",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)


spec_ot <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(0,1), include.mean = FALSE),
  distribution.model = "sstd"
)
spec_ot2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "nig",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ar2 = 0,
    ar3 = 0,
    ar4 = 0
  )
)
spec_ot3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,5), include.mean = FALSE),
  distribution.model = "ghyp",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)


uspec.n = multispec(c(spec_alt, spec_ot, spec_ot2, spec_ot3))


multf = multifit(uspec.n, multi_data)
multf

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution = "mvt")
fit1 = dccfit(spec1, data= multi_data, fit.control = list(eval.se=TRUE), fit=multf)
fit1

cov1 = rcov(fit1) # Extracts the covariance matrix
cor1 = rcor(fit1) # Extracts the correlation matrix

dim(cor1) # Nrow | Ncol | Nobs

cor1[,,dim(cor1)[3]] 


cor_tc <- cor1[2,1,] #Correlación entre la segunda fila y primera columna
cor_tc <- xts(cor_tc, order.by = Ret$Fecha)
# Plots
plot(cor_tc) # Correlación dinámica

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,], order.by = Ret$Fecha),main="COLCAP_Colombia and TC")
plot(as.xts(cor1[1,3,], order.by = Ret$Fecha),main="COLCAP_Colombia and SP500")
plot(as.xts(cor1[1,4,], order.by = Ret$Fecha),main="COLCAP_Colombia and VIX")

#GARCH por cada serie

lista_vols <- lapply(multf@fit, sigma)
vols_combinadas <- do.call(cbind, lista_vols)
colnames(vols_combinadas) <- c("COLCAP_Colombia", "TC_Colombia", "SP500_r", "VIX_r")
vols_combinadas <- xts(vols_combinadas, order.by = Ret$Fecha)

df_plot <- data.frame(
  Fecha = index(vols_combinadas),
  coredata(vols_combinadas)
)
df_largo <- pivot_longer(df_plot, 
                         cols = -Fecha, 
                         names_to = "Serie", 
                         values_to = "Volatilidad")

ggplot(df_largo, aes(x = Fecha, y = Volatilidad, color = Serie)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Serie, ncol = 1, scales = "free_y")
  labs(title = "Volatilidad Condicional (GARCH) de las 4 Series",
       x = "Tiempo",
       y = "Volatilidad (Sigma_t)") +
  theme_bw() +
  theme(legend.position = "bottom")
###############################################
  
####################################################
# Construcción de naive indicators
####################################################
dcc_cor <- rcor(fit1)
fechas <- Ret$Fecha
nombres <- colnames(multi_data)

# Configurar la ventana gráfica para tener 3 gráficos apilados (Verticalmente)
par(mfrow = c(3, 1), mar = c(2, 4, 2, 1)) # Ajusta márgenes

# Bucle para comparar Activo 1 contra Activo 2, 3 y 4
for (j in 2:4) {
  
  # --- A. Extraer DCC rho para el par (1, j) ---
  rho_dcc <- xts(dcc_cor[1, j, ], order.by = fechas)
  
  # --- B. Calcular Rolling Correlation (Naive) para el par (1, j) ---
  # Ventana de 90 días (ajusta si prefieres 60 o 120)
  rho_roll <- rollapply(multi_data, width = 90, 
                        function(x) cor(x[,1], x[,j]), 
                        by.column = FALSE, align = "right")
  rho_roll_zoo <- zoo(as.numeric(rho_roll), order.by = fechas) 
  rho_dcc_zoo  <- zoo(as.numeric(rho_dcc), order.by = fechas)
  # --- C. Graficar ---
  # Rango del eje Y entre -1 y 1 para ver bien la correlación
  plot(rho_roll_zoo, type = "l", col = "darkgray", lwd = 3, ylim = c(-1, 1),
       main = paste("Correlación Dinámica:", nombres[1], "vs", nombres[j]),
       ylab = "Correlación", xlab = "Año")
  
  # Añadir la línea del DCC encima (en azul o rojo)
  lines(rho_dcc_zoo, col = "blue", lwd = 1.5)
  
  # Añadir una línea horizontal en 0 como referencia
  abline(h = 0, lty = 2, col = "black")
  
  # Leyenda (solo en el primer gráfico para no ensuciar)
  if (j == 2) {
    legend("bottomright", legend = c("Rolling (90 días)", "DCC-GARCH"),
           col = c("darkgray", "blue"), lwd = c(1.5, 2), bty = "n", cex = 0.8)
  }
}

# Restaurar configuración gráfica
par(mfrow = c(1, 1))

#################### Descriptivos de los resultados #######################
dcc_cor <- rcor(fit1)
nombres <- colnames(multi_data) # Para usar los nombres correctos de tus series

# 2. Crear un data.frame solo con los pares que te interesan
# Asumiendo que la Columna 1 es BVL (tu activo principal)
df_correlaciones <- data.frame(
  "BVL_vs_TC"    = dcc_cor[1, 2, ],
  "BVL_vs_SP500" = dcc_cor[1, 3, ],
  "BVL_vs_VIX"   = dcc_cor[1, 4, ]
)

# 3. Definir función para calcular los estadísticos que pide una tesis
# Puedes agregar Skewness o Kurtosis si usas el paquete 'moments', 
# pero estos 5 son los esenciales.
calcular_stats <- function(x) {
  c(
    Media     = mean(x),
    Mediana   = median(x),
    Maximo    = max(x),
    Minimo    = min(x),
    Desv.Std  = sd(x)
  )
}

# 4. Generar la tabla
# 'sapply' aplica la función a cada columna
# 't()' transpone la tabla para que las series queden en filas (más legible)
tabla_resumen <- t(sapply(df_correlaciones, calcular_stats))

# 5. Mostrar la tabla redondeada a 4 decimales
print(round(tabla_resumen, 4))


####################################################
# Multi IPC_Mexico_r
####################################################
multi_data <- Retornos[,c("IPC_Mexico_r","TC_Mexico_r","SP500_r","VIX_r")]

  
spec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)

spec_ot <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  distribution.model = "sstd"
)
spec_ot2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE),
  distribution.model = "nig",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0,
    ar2 = 0,
    ar3 = 0,
    ar4 = 0
  )
)
spec_ot3 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0,5), include.mean = FALSE),
  distribution.model = "ghyp",
  fixed.pars = list(
    ma2 = 0, 
    ma3 = 0,
    ma4 = 0
  )
)


uspec.n = multispec(c(spec_alt, spec_ot, spec_ot2, spec_ot3))


multf = multifit(uspec.n, multi_data)
multf

spec1 = dccspec(uspec = uspec.n, dccOrder = c(1,1), distribution = "mvt")
fit1 = dccfit(spec1, data= multi_data, fit.control = list(eval.se=TRUE), fit=multf)
fit1

cov1 = rcov(fit1) # Extracts the covariance matrix
cor1 = rcor(fit1) # Extracts the correlation matrix

dim(cor1) # Nrow | Ncol | Nobs

cor1[,,dim(cor1)[3]] 


cor_tc <- cor1[2,1,] #Correlación entre la segunda fila y primera columna
cor_tc <- xts(cor_tc, order.by = Ret$Fecha)
# Plots
plot(cor_tc) # Correlación dinámica

par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor1[1,2,], order.by = Ret$Fecha),main="IPC_Mexico and TC")
plot(as.xts(cor1[1,3,], order.by = Ret$Fecha),main="IPC_Mexico and SP500")
plot(as.xts(cor1[1,4,], order.by = Ret$Fecha),main="IPC_Mexico and VIX")


#GARCH por cada serie

lista_vols <- lapply(multf@fit, sigma)
vols_combinadas <- do.call(cbind, lista_vols)
colnames(vols_combinadas) <- c("IPC_Mexico", "TC_Mexico", "SP500_r", "VIX_r")
vols_combinadas <- xts(vols_combinadas, order.by = Ret$Fecha)

df_plot <- data.frame(
  Fecha = index(vols_combinadas),
  coredata(vols_combinadas)
)
df_largo <- pivot_longer(df_plot, 
                         cols = -Fecha, 
                         names_to = "Serie", 
                         values_to = "Volatilidad")

ggplot(df_largo, aes(x = Fecha, y = Volatilidad, color = Serie)) +
  geom_line(alpha = 0.8) +
  facet_wrap(~ Serie, ncol = 1, scales = "free_y")
labs(title = "Volatilidad Condicional (GARCH) de las 4 Series",
     x = "Tiempo",
     y = "Volatilidad (Sigma_t)") +
  theme_bw() +
  theme(legend.position = "bottom")

###############################################################

####################################################
# Construcción de naive indicators
####################################################
dcc_cor <- rcor(fit1)
fechas <- Ret$Fecha
nombres <- colnames(multi_data)

# Configurar la ventana gráfica para tener 3 gráficos apilados (Verticalmente)
par(mfrow = c(3, 1), mar = c(2, 4, 2, 1)) # Ajusta márgenes

# Bucle para comparar Activo 1 contra Activo 2, 3 y 4
for (j in 2:4) {
  
  # --- A. Extraer DCC rho para el par (1, j) ---
  rho_dcc <- xts(dcc_cor[1, j, ], order.by = fechas)
  
  # --- B. Calcular Rolling Correlation (Naive) para el par (1, j) ---
  # Ventana de 90 días (ajusta si prefieres 60 o 120)
  rho_roll <- rollapply(multi_data, width = 90, 
                        function(x) cor(x[,1], x[,j]), 
                        by.column = FALSE, align = "right")
  rho_roll_zoo <- zoo(as.numeric(rho_roll), order.by = fechas) 
  rho_dcc_zoo  <- zoo(as.numeric(rho_dcc), order.by = fechas)
  # --- C. Graficar ---
  # Rango del eje Y entre -1 y 1 para ver bien la correlación
  plot(rho_roll_zoo, type = "l", col = "darkgray", lwd = 3, ylim = c(-1, 1),
       main = paste("Correlación Dinámica:", nombres[1], "vs", nombres[j]),
       ylab = "Correlación", xlab = "Año")
  
  # Añadir la línea del DCC encima (en azul o rojo)
  lines(rho_dcc_zoo, col = "blue", lwd = 1.5)
  
  # Añadir una línea horizontal en 0 como referencia
  abline(h = 0, lty = 2, col = "black")
  
  # Leyenda (solo en el primer gráfico para no ensuciar)
  if (j == 2) {
    legend("bottomright", legend = c("Rolling (90 días)", "DCC-GARCH"),
           col = c("darkgray", "blue"), lwd = c(1.5, 2), bty = "n", cex = 0.8)
  }
}

# Restaurar configuración gráfica
par(mfrow = c(1, 1))

#################### Descriptivos de los resultados #######################
dcc_cor <- rcor(fit1)
nombres <- colnames(multi_data) # Para usar los nombres correctos de tus series

# 2. Crear un data.frame solo con los pares que te interesan
# Asumiendo que la Columna 1 es BVL (tu activo principal)
df_correlaciones <- data.frame(
  "BVL_vs_TC"    = dcc_cor[1, 2, ],
  "BVL_vs_SP500" = dcc_cor[1, 3, ],
  "BVL_vs_VIX"   = dcc_cor[1, 4, ]
)

# 3. Definir función para calcular los estadísticos que pide una tesis
# Puedes agregar Skewness o Kurtosis si usas el paquete 'moments', 
# pero estos 5 son los esenciales.
calcular_stats <- function(x) {
  c(
    Media     = mean(x),
    Mediana   = median(x),
    Maximo    = max(x),
    Minimo    = min(x),
    Desv.Std  = sd(x)
  )
}

# 4. Generar la tabla
# 'sapply' aplica la función a cada columna
# 't()' transpone la tabla para que las series queden en filas (más legible)
tabla_resumen <- t(sapply(df_correlaciones, calcular_stats))

# 5. Mostrar la tabla redondeada a 4 decimales
print(round(tabla_resumen, 4))


################################################################################
# Cálculo del Ratio Sharpe post-estimación
################################################################################

# Asumiendo que 'dcc_fit' es tu objeto ajustado con dccfit()
# 1. Extraer la volatilidad condicional (Sigma)
# El objeto devuelve una matriz xts, asegúrate de tomar la columna del índice del país.
conditional_sigma <- sigma(fit1)

# 2. Convertir a Data Frame y guardar
datos_exportar <- data.frame(
  conditional_sigma
)

write.csv(datos_exportar, "Datos_Sharpe_MILA.csv", row.names = FALSE)



