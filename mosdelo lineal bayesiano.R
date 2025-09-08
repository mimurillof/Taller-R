# 1. Cargar la librería que contiene el conjunto de datos
library("car")

# 2. Cargar el conjunto de datos Anscombe en el entorno de trabajo
data("Anscombe")

# 3. Ajustar el modelo lineal
# La variable de respuesta es 'education' y los predictores son las otras tres:
# 'income', 'young' y 'urban'. El punto (.) es un atajo para incluir
# todas las demás variables del dataframe como predictores.
modelo <- lm(education ~ ., data = Anscombe)

# 4. Ver el resumen de los resultados del modelo
summary(modelo)

# Para obtener solo el valor del intercepto
# coef(modelo)[1]