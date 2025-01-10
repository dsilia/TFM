
install.packages("dplyr")
install.packages("tidyverse")
install.packages("demography")
library(dplyr)
library(tidyverse)
library(demography)

# Cargar los datos

library(readr)
poblacion <- read.csv("Poblacion.csv")
View(poblacion)
defunciones <- read.csv("defunciones.csv")
View(defunciones)


# Filtrar datos para el rango de edad para los años 2016 a 2022
poblacion <- poblacion %>% filter(Ano >= 2016, Ano <= 2022)
defunciones <- defunciones %>% filter(Ano >= 2016, Ano <= 2022)


# Crear una estructura de datos adecuada
years <- unique(poblacion$Ano)
ages <- unique(poblacion$Edad)

# Crear matrices para población y defunciones (Españoles)
pop_matrix_esp <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_esp <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_esp[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Espana") %>% pull(lx)
    death_matrix_esp[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Espana") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion Española
qx_esp<- death_matrix_esp / pop_matrix_esp

plot(log(qx_esp[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx española")
lines(log(qx_esp[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_esp[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_esp[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_esp[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_esp[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_esp[,7]), type="l",lwd=1.5, col = "hotpink")
legend("topright", legend = paste("colectivo 1", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_esp<- matrix(NA, nrow=nrow(qx_esp), ncol=ncol(qx_esp))

#iteramos cada columna(año)
for (j in 1:ncol(qx_esp)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_esp[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_esp[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_esp<-exp(qx_suavizadas_esp)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_esp[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años España", ylim = range(log(qx_esp)))

for (j in 1:ncol(qx_esp)) {
  # Original
  #lines(0:90, log(qx_esp[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_esp[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("topright", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 1 qx
datos_esp_suavizado <- demogdata(
  data = qx_suavizadas_esp,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_esp,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Española suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_esp_suavizado <- lca(datos_esp_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_esp_suavizado, main = "Main effects Lee-Carter españa 
     suavizado")
summary(lc_model_esp_suavizado)

# Calcular residuales
residuals_lca_esp_suavizado <- residuals(lc_model_esp_suavizado)

plot(residuals_lca_esp_suavizado, main = "Residuales del Modelo Lee-Carter España", xlab = "Edad", ylab = "Residuales")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años 
#predicciones_esp_5 <- forecast(lc_model_esp, h = 5)
#predicciones_esp_10 <- forecast(lc_model_esp, h = 10)
predicciones_esp_suavizado_15 <- forecast(lc_model_esp_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_esp_5)
#plot(predicciones_esp_10)
plot(predicciones_esp_suavizado_15, main= "España suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_esp_5 <- predicciones_esp_5$rate$total
#tasas_futuras_esp_10 <- predicciones_esp_10$rate$total
tasas_futuras_esp_suavizado_15 <- predicciones_esp_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_esp_5
#tasas_futuras_esp_10
tasas_futuras_esp_suavizado_15


# Crear un data frame con las edades y los años para España
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años)

# Reestructuramos la matriz para crear un dataframe largo
df_esp_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_esp_suavizado_15)  # Convertimos la matriz en un vector
)


#---------------------------------------------------------------------------
# Colectivo 1
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 1)
pop_matrix_colec_1 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_1 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_1[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "UE 28") %>% pull(lx)
    death_matrix_colec_1[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "UE 28") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 1
qx_colec_1<- death_matrix_colec_1 / pop_matrix_colec_1

plot(log(qx_colec_1[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 1")
lines(log(qx_colec_1[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_1[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_1[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_1[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_1[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_1[,7]), type="l",lwd=1.5, col = "hotpink")
legend("topright", legend = paste("colectivo 1", 2016:2022),
  col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_1<- matrix(NA, nrow=nrow(qx_colec_1), ncol=ncol(qx_colec_1))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_1)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_1[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_colec_1[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_1<-exp(qx_suavizadas_colec_1)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_1[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años", ylim = range(log(qx_colec_1)))

for (j in 1:ncol(qx_colec_1)) {
  # Original
  #lines(0:90, log(qx_colec_1[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_1[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("topright", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 1 qx
datos_colec_1_suavizado <- demogdata(
  data = qx_suavizadas_colec_1,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_1,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 1 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_1_suavizado <- lca(datos_colec_1_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_1_suavizado, main = "Main effects Lee-Carter Colectivo 1 
     suavizado")


# Calcular residuales
residuals_lca_colec_1_suavizado <- residuals(lc_model_colec_1_suavizado)

plot(residuals_lca_colec_1_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 1", xlab = "Edad", ylab = "Residuales")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_1_5 <- forecast(lc_model_colec_1, h = 5)
#predicciones_colec_1_10 <- forecast(lc_model_colec_1, h = 10)
predicciones_colec_1_suavizado_15 <- forecast(lc_model_colec_1_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_1_5)
#plot(predicciones_colec_1_10)
plot(predicciones_colec_1_suavizado_15, main= "Colectivo 1 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_1_5 <- predicciones_colec_1_5$rate$total
#tasas_futuras_colec_1_10 <- predicciones_colec_1_10$rate$total
tasas_futuras_colec_1_suavizado_15 <- predicciones_colec_1_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_1_5
#tasas_futuras_colec_1_10
tasas_futuras_colec_1_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 1
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años)

# Reestructuramos la matriz para crear un dataframe largo
df_colec_1_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_1_suavizado_15)  # Convertimos la matriz en un vector
  )




#---------------------------------------------------------------------------
# Colectivo 2
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 2)
pop_matrix_colec_2 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_2 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_2[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Africa") %>% pull(lx)
    death_matrix_colec_2[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Africa") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 2
qx_colec_2<- death_matrix_colec_2 / pop_matrix_colec_2


plot(log(qx_colec_2[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 2")
lines(log(qx_colec_2[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_2[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_2[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_2[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_2[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_2[,7]), type="l",lwd=1.5, col = "hotpink")
#legend("right", legend = paste("colectivo 2", 2016:2022),
       #col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)
# Añadir la leyenda debajo del gráfico
legend("topright", legend = paste("colectivo 2", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1, lwd=2, inset=c(0, -0.4), xpd=TRUE)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_2<- matrix(NA, nrow=nrow(qx_colec_2), ncol=ncol(qx_colec_2))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_2)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_2[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_colec_2[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_2<-exp(qx_suavizadas_colec_2)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_2[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años colectivo 2", ylim = range(log(qx_colec_2)))

for (j in 1:ncol(qx_colec_2)) {
  # Original
  #lines(0:90, log(qx_colec_1[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_2[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("left", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2, cex = 0.74)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 2 qx
datos_colec_2_suavizado <- demogdata(
  data = qx_suavizadas_colec_2,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_2,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 2 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_2_suavizado <- lca(datos_colec_2_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_2_suavizado, main = "Main effects Lee-Carter Colectivo 2 
     suavizado")

par(mar = c(5, 4, 4, 2) + 0.1)  # Ajusta los márgenes según sea necesario
plot(lc_model_colec_2_suavizado, main = "Main effects Lee-Carter Colectivo 2 
     suavizado")

# Calcular residuales
residuals_lca_colec_2_suavizado <- residuals(lc_model_colec_2_suavizado)
par(mar = c(0.55, 2, 2, 2) + 0.1)
plot(residuals_lca_colec_2_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 2", xlab = "Edad", ylab = "Residuales")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_2_5 <- forecast(lc_model_colec_2, h = 5)
#predicciones_colec_2_10 <- forecast(lc_model_colec_2, h = 10)
predicciones_colec_2_suavizado_15 <- forecast(lc_model_colec_2_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_2_5)
#plot(predicciones_colec_2_10)
par(mar = c(0.55, 2, 2, 2) + 0.1)
plot(predicciones_colec_2_suavizado_15, main= "Colectivo 2 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_2_5 <- predicciones_colec_2_5$rate$total
#tasas_futuras_colec_2_10 <- predicciones_colec_2_10$rate$total
tasas_futuras_colec_2_suavizado_15 <- predicciones_colec_2_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_2_5
#tasas_futuras_colec_2_10
tasas_futuras_colec_2_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 1
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años)

# Reestructuramos la matriz para crear un dataframe largo
df_colec_2_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_2_suavizado_15)  # Convertimos la matriz en un vector
)




#---------------------------------------------------------------------------
# Colectivo 3
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 3)
pop_matrix_colec_3 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_3 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_3[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Sudamerica") %>% pull(lx)
    death_matrix_colec_3[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Sudamerica") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 3
qx_colec_3<- death_matrix_colec_3 / pop_matrix_colec_3



plot(log(qx_colec_3[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 3")
lines(log(qx_colec_3[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_3[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_3[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_3[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_3[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_3[,7]), type="l",lwd=1.5, col = "hotpink")
#legend("right", legend = paste("colectivo 3", 2016:2022),
#col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)
# Añadir la leyenda debajo del gráfico
legend("topright", legend = paste("colectivo 3", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1, lwd=2, inset=c(0, -0.4), xpd=TRUE)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_3<- matrix(NA, nrow=nrow(qx_colec_3), ncol=ncol(qx_colec_3))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_3)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_3[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_colec_3[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_3<-exp(qx_suavizadas_colec_3)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_3[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años colectivo 3", ylim = range(log(qx_colec_3)))

for (j in 1:ncol(qx_colec_3)) {
  # Original
  #lines(0:90, log(qx_colec_3[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_3[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("left", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2, cex = 0.999)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 3 qx
datos_colec_3_suavizado <- demogdata(
  data = qx_suavizadas_colec_3,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_3,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 3 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_3_suavizado <- lca(datos_colec_3_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_3_suavizado, main = "Main effects Lee-Carter Colectivo 3 
     suavizado")

# Calcular residuales
residuals_lca_colec_3_suavizado <- residuals(lc_model_colec_3_suavizado)

plot(residuals_lca_colec_3_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 3", xlab = "Edad", ylab = "Residuales")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_3_5 <- forecast(lc_model_colec_3, h = 5)
#predicciones_colec_3_10 <- forecast(lc_model_colec_3, h = 10)
predicciones_colec_3_suavizado_15 <- forecast(lc_model_colec_3_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_3_5)
#plot(predicciones_colec_3_10)
par(mar = c(0.55, 2, 2, 2) + 0.1)
plot(predicciones_colec_3_suavizado_15, main= "Colectivo 3 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_3_5 <- predicciones_colec_3_5$rate$total
#tasas_futuras_colec_3_10 <- predicciones_colec_3_10$rate$total
tasas_futuras_colec_3_suavizado_15 <- predicciones_colec_3_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_3_5
#tasas_futuras_colec_3_10
tasas_futuras_colec_3_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 3
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años 

# Reestructuramos la matriz para crear un dataframe largo
df_colec_3_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_3_suavizado_15)  # Convertimos la matriz en un vector
)



#---------------------------------------------------------------------------
# Colectivo 4
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 4)
pop_matrix_colec_4 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_4 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_4[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Centroamerica") %>% pull(lx)
    death_matrix_colec_4[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Centroamerica") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 4
qx_colec_4<- death_matrix_colec_4 / pop_matrix_colec_4

plot(log(qx_colec_4[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 4")
lines(log(qx_colec_4[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_4[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_4[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_4[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_4[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_4[,7]), type="l",lwd=1.5, col = "hotpink")
#legend("right", legend = paste("colectivo 4", 2016:2022),
#col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)
# Añadir la leyenda debajo del gráfico
legend("topright", legend = paste("colectivo 4", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1, lwd=2, inset=c(0, -0.4), xpd=TRUE)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_4<- matrix(NA, nrow=nrow(qx_colec_4), ncol=ncol(qx_colec_4))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_4)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_4[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_colec_4[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_4<-exp(qx_suavizadas_colec_4)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_4[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años colectivo 4", ylim = range(log(qx_colec_4)))

for (j in 1:ncol(qx_colec_4)) {
  # Original
  #lines(0:90, log(qx_colec_4[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_4[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("left", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2, cex = 0.999)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 4 qx
datos_colec_4_suavizado <- demogdata(
  data = qx_suavizadas_colec_4,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_4,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 4 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_4_suavizado <- lca(datos_colec_4_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_4_suavizado, main = "Main effects Lee-Carter Colectivo 4 
     suavizado")

# Calcular residuales
residuals_lca_colec_4_suavizado <- residuals(lc_model_colec_4_suavizado)

plot(residuals_lca_colec_4_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 4", xlab = "Edad", ylab = "Residuales")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_4_5 <- forecast(lc_model_colec_4, h = 5)
#predicciones_colec_4_10 <- forecast(lc_model_colec_4, h = 10)
predicciones_colec_4_suavizado_15 <- forecast(lc_model_colec_4_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_4_5)
#plot(predicciones_colec_4_10)
plot(predicciones_colec_4_suavizado_15, main= "Colectivo 4 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_4_5 <- predicciones_colec_4_5$rate$total
#tasas_futuras_colec_4_10 <- predicciones_colec_4_10$rate$total
tasas_futuras_colec_4_suavizado_15 <- predicciones_colec_4_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_4_5
#tasas_futuras_colec_4_10
tasas_futuras_colec_4_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 4
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años 

# Reestructuramos la matriz para crear un dataframe largo
df_colec_4_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_4_suavizado_15)  # Convertimos la matriz en un vector
)



#---------------------------------------------------------------------------
# Colectivo 5
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 5)
pop_matrix_colec_5 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_5 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_5[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Norteamerica") %>% pull(lx)
    death_matrix_colec_5[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Norteamerica") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 5
qx_colec_5<- death_matrix_colec_5 / pop_matrix_colec_5

plot(log(qx_colec_5[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 5", ylim = c(-8.5, -1))
lines(log(qx_colec_5[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_5[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_5[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_5[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_5[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_5[,7]), type="l",lwd=1.5, col = "hotpink")
#legend("right", legend = paste("colectivo 5", 2016:2022),
#col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)
# Añadir la leyenda debajo del gráfico
legend("topright", legend = paste("colectivo 5", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1, lwd=2, inset=c(0, -0.4), xpd=TRUE)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_5<- matrix(NA, nrow=nrow(qx_colec_5), ncol=ncol(qx_colec_5))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_5)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_5[,j]),spar = 0.5) #eje x edades
  qx_suavizadas_colec_5[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_5<-exp(qx_suavizadas_colec_5)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_5[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años colectivo 5", ylim = range(log(qx_colec_5)))

for (j in 1:ncol(qx_colec_5)) {
  # Original
  #lines(0:90, log(qx_colec_5[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_5[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("left", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2, cex = 0.999)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 5 qx
datos_colec_5_suavizado <- demogdata(
  data = qx_suavizadas_colec_5,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_5,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 5 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_5_suavizado <- lca(datos_colec_5_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_5_suavizado, main = "Main effects Lee-Carter Colectivo 5 
     suavizado")

# Calcular residuales
residuals_lca_colec_5_suavizado <- residuals(lc_model_colec_5_suavizado)

plot(residuals_lca_colec_5_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 5", xlab = "Años", ylab = "Edades")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_5_5 <- forecast(lc_model_colec_5, h = 5)
#predicciones_colec_5_10 <- forecast(lc_model_colec_5, h = 10)
predicciones_colec_5_suavizado_15 <- forecast(lc_model_colec_5_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_5_5)
#plot(predicciones_colec_5_10)
plot(predicciones_colec_5_suavizado_15, main= "Colectivo 5 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_5_5 <- predicciones_colec_5_5$rate$total
#tasas_futuras_colec_5_10 <- predicciones_colec_5_10$rate$total
tasas_futuras_colec_5_suavizado_15 <- predicciones_colec_5_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_5_5
#tasas_futuras_colec_5_10
tasas_futuras_colec_5_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 5
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años 

# Reestructuramos la matriz para crear un dataframe largo
df_colec_5_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_5_suavizado_15)  # Convertimos la matriz en un vector
)




#---------------------------------------------------------------------------
# Colectivo 6
#---------------------------------------------------------------------------

# Crear matrices para población y defunciones (Colectivo 6)
pop_matrix_colec_6 <- matrix(NA, nrow = length(ages), ncol = length(years))
death_matrix_colec_6 <- matrix(NA, nrow = length(ages), ncol = length(years))

for (i in 1:length(ages)) {
  for (j in 1:length(years)) {
    pop_matrix_colec_6[i, j] <- poblacion %>% filter(Edad == ages[i], Ano == years[j], Origen == "Asia") %>% pull(lx)
    death_matrix_colec_6[i, j] <- defunciones %>% filter(Edad == ages[i], Ano == years[j], Origen == "Asia") %>% pull(dx)
  }
}


#calculamos las qx para la poblacion perteneciente al colectivo 6
qx_colec_6<- death_matrix_colec_6 / pop_matrix_colec_6


plot(log(qx_colec_6[,1]), type="l", col = "red", lwd=1.5, xlab = "Edad", ylab = "Log qx", main = "log qx colectivo 6", ylim = c(-9.5, -2))
lines(log(qx_colec_6[,2]), type="l",lwd=1.5, col = "green")
lines(log(qx_colec_6[,3]), type="l",lwd=1.5, col = "blue")
lines(log(qx_colec_6[,4]), type="l",lwd=1.5, col = "orange")
lines(log(qx_colec_6[,5]), type="l",lwd=1.5, col = "brown")
lines(log(qx_colec_6[,6]), type="l",lwd=1.5, col = "yellow")
lines(log(qx_colec_6[,7]), type="l",lwd=1.5, col = "hotpink")
#legend("right", legend = paste("colectivo 6", 2016:2022),
#col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1,lwd =2)
# Añadir la leyenda debajo del gráfico
legend("topright", legend = paste("colectivo 6", 2016:2022),
       col= c("red","green","blue","orange","brown","yellow","hotpink"), lty=1, lwd=2, inset=c(0, -0.4), xpd=TRUE)

#el recorrido es muy erratico aplicaremos spline
#creamos un objeto para almacenar los resultados suavizados
qx_suavizadas_colec_6<- matrix(NA, nrow=nrow(qx_colec_6), ncol=ncol(qx_colec_6))

#iteramos cada columna(año)
for (j in 1:ncol(qx_colec_6)){
  #Aplicar smooth.spline al log de cada columna
  spline_fit<-smooth.spline(0:90, log(qx_colec_6[,j])) #eje x edades
  qx_suavizadas_colec_6[,j] <- spline_fit$y        #guardar valore suavizados
}

#volvemos a transformar los log(qx) a qx los datos suavizados
qx_suavizadas_colec_6<-exp(qx_suavizadas_colec_6)


# Graficar curvas originales y suavizadas para todos los años
plot(0:90, log(qx_colec_6[, 1]), type = "n", 
     xlab = "Edad", ylab = "Log(qx)", main = "Splines para Todos los Años colectivo 6", ylim = c(-9,-2))

for (j in 1:ncol(qx_colec_6)) {
  # Original
  #lines(0:90, log(qx_colec_6[, j]), col = rainbow(7)[j], lty = 2)
  # Suavizada
  lines(0:90, log(qx_suavizadas_colec_6[, j]), col = rainbow(7)[j], lwd = 2)
}

legend("left", legend = paste("Año", 2016:2022), col = rainbow(7), lty = 1, lwd = 2, cex = 0.99)


# Crear el objeto demogdata usando las tasas de mortalidad del colectivo 6 qx
datos_colec_6_suavizado <- demogdata(
  data = qx_suavizadas_colec_6,                 # Tasa de mortalidad qx calculada
  pop = pop_matrix_colec_6,          # Matriz de población
  ages = ages,                   # Vector de edades
  years = years,                 # Vector de años
  type = "mortality",            # Tipo de datos: mortalidad
  label = "Colectivo 6 suavizado",              # Etiqueta para identificar el conjunto de datos
  name = "total"
)

# Aplicar el modelo Lee-Carter
lc_model_colec_6_suavizado <- lca(datos_colec_6_suavizado , adjust = "none", scale = TRUE) 
plot(lc_model_colec_6_suavizado, main = "Main effects Lee-Carter Colectivo 6 
     suavizado")

# Calcular residuales
residuals_lca_colec_6_suavizado <- residuals(lc_model_colec_6_suavizado)

plot(residuals_lca_colec_6_suavizado, main = "Residuales del Modelo Lee-Carter Colectivo 6", xlab = "Años", ylab = "Edades")

# Predecir tasas de mortalidad para los próximos 5 años, 10 años, 15 años (por ejemplo)
#predicciones_colec_6_5 <- forecast(lc_model_colec_6, h = 5)
#predicciones_colec_6_10 <- forecast(lc_model_colec_6, h = 10)
#predicciones_colec_6_suavizado_15 <- forecast(lc_model_colec_6_suavizado, h = 15)

# Visualizar las predicciones
#plot(predicciones_colec_6_5)
#plot(predicciones_colec_6_10)
plot(predicciones_colec_6_suavizado_15, main= "Colectivo 6 suavizado: Total Death Rates (2023-2037)")

# Ver las tasas de mortalidad proyectadas
#tasas_futuras_colec_6_5 <- predicciones_colec_6_5$rate$total
#tasas_futuras_colec_6_10 <- predicciones_colec_6_10$rate$total
tasas_futuras_colec_6_suavizado_15 <- predicciones_colec_6_suavizado_15$rate$total


# Mostrar las tasas proyectadas
#tasas_futuras_colec_6_5
#tasas_futuras_colec_6_10
tasas_futuras_colec_6_suavizado_15


# Crear un data frame con las edades y los años para el colectivo 6
edades <- seq(0, 90, by = 1)  # Vector de edades
anios <- seq(1, 15, by = 1)    # Vector de 15 años 

# Reestructuramos la matriz para crear un dataframe largo
df_colec_6_suavizado <- data.frame(
  Edad = rep(edades, times = length(anios)),
  Anio = rep(anios, each = length(edades)),
  qx_prediccion = as.vector(tasas_futuras_colec_6_suavizado_15)  # Convertimos la matriz en un vector
)



#----------------------------------------------------------
#KL divergence
#---------------------------------------------------------

# Función para calcular KL Divergence
kl_divergence <- function(p, q) {
  p <- p / sum(p)  # Normalizar P
  q <- q / sum(q)  # Normalizar Q
  sum(p * log(p / q), na.rm = TRUE)  # Calcular KL Divergence
}

# Lista de data.frames de los colectivos
colectivos <- list(
  df_colec_1_suavizado,
  df_colec_2_suavizado,
  df_colec_3_suavizado,
  df_colec_4_suavizado,
  df_colec_5_suavizado,
  df_colec_6_suavizado)

# Crear un data.frame para guardar resultados p=colectivos;q=españa
resultados_kl <- data.frame()

# Iterar por cada colectivo
for (i in seq_along(colectivos)) {
  df_colec <- colectivos[[i]]
  
  # Filtrar datos para 15 años por seguridad
  df_colec_15 <- subset(df_colec, Anio <= 15)
  df_esp_15 <- subset(df_esp_suavizado, Anio <= 15)
  
  # Calcular KL para cada año
  for (year in unique(df_colec_15$Anio)) {
    # Distribuciones para el año actual
    p <- df_colec_15$qx_prediccion[df_colec_15$Anio == year]
    q <- df_esp_15$qx_prediccion[df_esp_15$Anio == year]
    
    # Calcular KL Divergence
    kl_value <- kl_divergence(p, q)
    
    # Guardar resultados
    resultados_kl <- rbind(resultados_kl, data.frame(
      colectivo = paste0("Colectivo_", i),
      Anio = year,
      kl_divergence = kl_value
    ))
  }
}

# Ver resultados
print(resultados_kl)

write.csv(resultados_kl,"resultado kl.csv", row.names = FALSE )



install.packages("ggplot2")
library(ggplot2)
ggplot(resultados_kl,aes(x=Anio, y= kl_divergence, colour = colectivo, group = colectivo))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "KL Divergence entre Colectivos y España",
    x="Año",
    y="KL Divergence")+
  theme_minimal()
  



#ahora cambiamos a p= españa y q=colectivos
resultados_kl_2 <- data.frame()

# Iterar por cada colectivo
for (i in seq_along(colectivos)) {
  df_colec <- colectivos[[i]]
  
  # Filtrar datos para 15 años por seguridad
  df_colec_15 <- subset(df_colec, Anio <= 15)
  df_esp_15 <- subset(df_esp_suavizado, Anio <= 15)
  
  # Calcular KL para cada año
  for (year in unique(df_colec_15$Anio)) {
    # Distribuciones para el año actual
    q <- df_colec_15$qx_prediccion[df_colec_15$Anio == year]
    p <- df_esp_15$qx_prediccion[df_esp_15$Anio == year]
    
    # Calcular KL Divergence
    kl_value <- kl_divergence(p, q)
    
    # Guardar resultados
    resultados_kl_2 <- rbind(resultados_kl_2, data.frame(
      colectivo = paste0("Colectivo_", i),
      Anio = year,
      kl_divergence = kl_value
    ))
  }
}

# Ver resultados
print(resultados_kl_2)

write.csv(resultados_kl_2,"resultado kl_2.csv", row.names = FALSE )



install.packages("ggplot2")
library(ggplot2)
ggplot(resultados_kl_2,aes(x=Anio, y= kl_divergence, colour = colectivo, group = colectivo))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  labs(
    title = "KL Divergence entre España y Colectivos",
    x="Año",
    y="KL Divergence")+
  theme_minimal()

#importamos los datos de primas calculadas a 5, 10 y 15 años
library(readxl)
Primas_5_10_15 <- read_excel("C:/Users/silia/OneDrive/Escritorio/datos tfm/Traspaso pc/Primas_5_10_15.xlsx")
View(Primas_5_10_15)

# Cargar librerías
library(ggplot2)
library(reshape2)

# Crear el data frame con los datos
Primas_5_10_15 <- data.frame(
  Población = c("España", "colectivo 1", "colectivo 2", "colectivo 3", "colectivo 4", "colectivo 5", "colectivo 6"),
  `5 años` = c(49292.84, 47368.31, 47017.74, 46592.98, 46298.33, 45975.34, 46371.39),
  `10 años` = c(47143.26, 47282.20, 47264.24, 46809.85, 46545.67, 45723.24, 46418.64),
  `15 años` = c(47165.14, 47199.82, 47545.29, 47062.45, 46861.89, 45563.40, 46468.85)
)

# Convertir el formato de la tabla a largo
Primas_long <- melt(Primas_5_10_15, id.vars = "Población", variable.name = "Plazo", value.name = "Prima")

# Crear el gráfico
ggplot(Primas_long, aes(x = Plazo, y = Prima, group = Población)) +
  # Línea de España en negro fuerte
  geom_line(data = subset(Primas_long, Población == "España"), color = "black", size = 1.2) +
  # Líneas de los colectivos en punteado y colores suaves
  geom_line(data = subset(Primas_long, Población != "España"),
            aes(color = Población, linetype = Población), size = 0.8) +
  scale_color_manual(
    values = c("blue", "green", "pink", "yellow", "coral", "cyan")
    # Colores suaves
  ) +
  scale_linetype_manual(
    values = c("dotted", "dotted", "dotted", "dotted", "dotted", "dotted")  # Líneas punteadas
  ) +
  ylim(45500, 50000)+
  theme_minimal() +
  labs(
    title = "Evolución de Primas a 5, 10 y 15 años",
    x = "Plazo",
    y = "Prima"
  ) +
  theme(legend.title = element_blank())


#importamos los datos del SCR mortalidad calculadas a 5, 10 y 15 años
library(readxl)
Primas_5_10_15 <- read_excel("C:/Users/silia/OneDrive/Escritorio/datos tfm/Traspaso pc/SCR_mor_5_10_15.xlsx")
View(SCR_mor_5_10_15)

# Cargar librerías
library(ggplot2)
library(reshape2)

# Crear el data frame con los datos
SCR_mor_5_10_15 <- data.frame(
  Población = c("España", "colectivo 1", "colectivo 2", "colectivo 3", "colectivo 4", "colectivo 5", "colectivo 6"),
  `5 años` = c(535.56, 283.21, 236.36, 179.04, 138.16, 94.79, 148.47),
  `10 años` = c(252.78, 271.85, 269.39, 208.52, 170.88, 60.29, 154.71),
  `15 años` = c(255.73, 260.96, 306.87, 242.74, 212.38, 38.32, 161.34)
)

# Convertir el formato de la tabla a largo
SCR_mor_long <- melt(SCR_mor_5_10_15, id.vars = "Población", variable.name = "Plazo", value.name = "SCR_Mortalidad")

# Crear el gráfico
ggplot(SCR_mor_long, aes(x = Plazo, y = SCR_Mortalidad, group = Población)) +
  # Línea de España en negro fuerte
  geom_line(data = subset(SCR_mor_long, Población == "España"), color = "black", size = 1.2) +
  # Líneas de los colectivos en punteado y colores suaves
  geom_line(data = subset(SCR_mor_long, Población != "España"),
            aes(color = Población, linetype = Población), size = 0.8) +
  scale_color_manual(
    values = c("blue", "green", "pink", "yellow", "coral", "cyan")
    # Colores suaves
  ) +
  scale_linetype_manual(
    values = c("dotted", "dotted", "dotted", "dotted", "dotted", "dotted")  # Líneas punteadas
  ) +
  ylim(30, 550)+
  theme_minimal() +
  labs(
    title = "Evolución del SCR Mortalidad a 5, 10 y 15 años",
    x = "Plazo",
    y = "SCR Mortalidad"
  ) +
  theme(legend.title = element_blank())




#importamos los datos del SCR longevidad calculadas a 5, 10 y 15 años
library(readxl)
Primas_5_10_15 <- read_excel("C:/Users/silia/OneDrive/Escritorio/datos tfm/Traspaso pc/SCR_mor_5_10_15.xlsx")
View(SCR_long_5_10_15)

# Cargar librerías
library(ggplot2)
library(reshape2)

# Crear el data frame con los datos
SCR_long_5_10_15 <- data.frame(
  Población = c("España", "colectivo 1", "colectivo 2", "colectivo 3", "colectivo 4", "colectivo 5", "colectivo 6"),
  `5 años` = c(-721.78, -379.68, -316.57, -239.54, -184.69, -126.61, -198.51),
  `10 años` = c(-338.68, -364.36, -361.05, -279.13, -228.59, -80.48, -206.89),
  `15 años` = c(-342.66, -349.69, -411.59, -325.15, -284.33, -51.13, -215.79)
)

# Convertir el formato de la tabla a largo
SCR_long_long <- melt(SCR_long_5_10_15, id.vars = "Población", variable.name = "Plazo", value.name = "SCR_Longevidad")

# Crear el gráfico
ggplot(SCR_long_long, aes(x = Plazo, y = SCR_Longevidad, group = Población)) +
  # Línea de España en negro fuerte
  geom_line(data = subset(SCR_long_long, Población == "España"), color = "black", size = 1.2) +
  # Líneas de los colectivos en punteado y colores suaves
  geom_line(data = subset(SCR_long_long, Población != "España"),
            aes(color = Población, linetype = Población), size = 0.8) +
  scale_color_manual(
    values = c("blue", "green", "pink", "yellow", "coral", "cyan")
    # Colores suaves
  ) +
  scale_linetype_manual(
    values = c("dotted", "dotted", "dotted", "dotted", "dotted", "dotted")  # Líneas punteadas
  ) +
  ylim(-50, -750)+
  theme_minimal() +
  labs(
    title = "Evolución del SCR longevidad a 5, 10 y 15 años",
    x = "Plazo",
    y = "SCR Longevidad"
  ) +
  theme(legend.title = element_blank())



#boxplot primas

# paquetes necesarios
library(tidyr)
library(ggplot2)

# Convertir la tabla a formato largo
primas_long <- Primas_esp_col %>%
  pivot_longer(cols = -`Año Proyectado`, # Todas las columnas excepto "años proyectados"
               names_to = "Grupo", 
               values_to = "Prima")

# Crear el boxplot
ggplot(primas_long, aes(x = Grupo, y = Prima, fill = Grupo)) +
  geom_boxplot() +
  labs(title = "Comparación de Primas entre España y los Colectivos",
       x = "Grupo",
       y = "Monto de Prima") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas en el eje X
