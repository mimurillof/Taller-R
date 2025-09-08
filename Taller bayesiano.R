# Taller de Regresión Lineal Bayesiana: Análisis de Datos de Golf
# Preparado para: Miguel Ángel Murillo Frías

# --- Paso 1: Carga y Preparación de Datos ---

# URL del conjunto de datos, como se especifica en el problema.
url_datos <- "http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat"

# Cargar los datos en un data frame. 
# No tiene encabezado, así que asignaremos nombres a las columnas.
datos_golf <- read.table(url_datos, header = FALSE, 
                         col.names = c("Distancia", "Precision", "Circuito"))

# Verificamos las primeras filas para asegurarnos de que se cargó correctamente.
# Circuito 1 = LPGA (Femenino), Circuito 2 = PGA (Masculino)
print("Primeras filas de los datos completos:")
head(datos_golf)

# Creamos un subconjunto solo con los datos de las golfistas (LPGA), 
# que es lo que pide el problema.
datos_lpga <- subset(datos_golf, Circuito == 1)

print("Primeras filas de los datos de las golfistas (LPGA):")
head(datos_lpga)


# --- Paso 2: Análisis Exploratorio (Pregunta 1) ---

# Crear el gráfico de dispersión para visualizar la relación.
plot(datos_lpga$Distancia, datos_lpga$Precision,
     main = "Relación entre Distancia y Precisión en LPGA",
     xlab = "Distancia Media de Drive (yardas)",
     ylab = "Porcentaje de Precisión (%)",
     pch = 19, # Círculos sólidos
     col = "blue")


# --- Paso 3: Ajuste del Modelo de Regresión Lineal (Pregunta 2) ---

# Ajustamos el modelo lineal simple. La fórmula `Precision ~ Distancia`
# significa que estamos modelando la Precisión en función de la Distancia.
# Esto corresponde a E(y) = b0 + b1*x.
modelo_lpga <- lm(Precision ~ Distancia, data = datos_lpga)

# Obtenemos el resumen completo del modelo.
# Aquí encontrarás el valor de la pendiente (b1) que necesitas.
print("--- Resultados del Modelo de Regresión (Pregunta 2) ---")
summary(modelo_lpga)


# --- Paso 4: Realizar Predicciones (Preguntas 4 y 5) ---

# Creamos un nuevo data frame con el valor para el cual queremos predecir.
nueva_distancia <- data.frame(Distancia = 260)

# Calculamos la predicción puntual (la media posterior predictiva).
prediccion_puntual <- predict(modelo_lpga, newdata = nueva_distancia)

print("--- Estimación Predictiva para x = 260 (Pregunta 4) ---")
# Usamos round() para redondear a 1 decimal, como pide la pregunta.
print(round(prediccion_puntual, 1))

# Calculamos el intervalo de predicción posterior del 95%.
intervalo_prediccion <- predict(modelo_lpga, newdata = nueva_distancia, 
                                interval = "prediction", level = 0.95)

print("--- Intervalo de Predicción del 95% (Pregunta 5) ---")
print(intervalo_prediccion)