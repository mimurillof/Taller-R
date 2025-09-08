# Cargar la librería rjags, necesaria para interactuar con JAGS
library("rjags")

# Cargar el conjunto de datos PlantGrowth
data("PlantGrowth") 

# Preparar los datos para JAGS
# La variable de respuesta 'y' será el peso de la planta
# La variable de grupo 'grp' se convierte a numérica (1, 2, 3) para JAGS
data_jags = list(y = PlantGrowth$weight,
                 grp = as.numeric(PlantGrowth$group))

# Cargar la librería rjags, necesaria para interactuar con JAGS
library("rjags")

# Cargar el conjunto de datos PlantGrowth
data("PlantGrowth") 

# Preparar los datos para JAGS
# La variable de respuesta 'y' será el peso de la planta
# La variable de grupo 'grp' se convierte a numérica (1, 2, 3) para JAGS
data_jags = list(y = PlantGrowth$weight,
                 grp = as.numeric(PlantGrowth$group))

# Definición del modelo 1 en sintaxis JAGS (como cadena de texto)
mod1_string = "
model {
    # Verosimilitud: Cada observación 'y[i]' sigue una distribución normal
    # cuya media depende del grupo 'grp[i]' y tiene una precisión única 'prec'.
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }

    # Priors (distribuciones a priori) para las medias de cada grupo
    # Se usan priors normales no informativos (media 0, varianza muy grande)
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }

    # Prior para la precisión (inversa de la varianza)
    # Se utiliza un prior Gamma (equivalente a un Inverse-Gamma para la varianza)
    prec ~ dgamma(5/2.0, 5*1.0/2.0)

    # Cantidad derivada: desviación estándar, para una interpretación más fácil
    sig = sqrt( 1.0 / prec )
} "

# Parámetros a monitorear para el Modelo 1
params1 = c("mu", "sig")

# Función para generar valores iniciales aleatorios para cada cadena
inits1 = function() {
  list("mu" = rnorm(3, 0.0, 100.0), # Medias iniciales aleatorias para cada grupo
       "prec" = rgamma(1, 1.0, 1.0)) # Precisión inicial aleatoria (única)
}

# Fijar semilla para reproducibilidad
set.seed(82)

# Compilar el Modelo 1
mod1 = jags.model(textConnection(mod1_string), data=data_jags, inits=inits1, n.chains=3)

# Fase de Burn-in (descartar las primeras iteraciones para asegurar la convergencia)
update(mod1, 1e3)

# Calcular el DIC para el Modelo 1 y guardarlo como 'dic1'
dic1 = dic.samples(mod1, n.iter=1e3)

# Definición del modelo 2 en sintaxis JAGS (con varianzas separadas)
mod2_string = "
model {
    # Verosimilitud: Cada observación 'y[i]' sigue una distribución normal
    # cuya media y precisión dependen del grupo específico 'grp[i]' al que pertenece.
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]]) # Precisión específica para cada grupo
    }

    # Priors para las medias de cada grupo (igual que en el Modelo 1)
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6) # Prior normal no informativo
    }

    # Priors independientes para la precisión de CADA GRUPO
    # Cada grupo tiene su propia precisión 'prec[j]' con su propio prior Gamma
    for (j in 1:3) {
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0) # Priors Gamma independientes
        sig[j] = sqrt(1.0 / prec[j]) # Desviación estándar derivada para cada grupo
    }
} "

# Parámetros a monitorear para el Modelo 2
# Ahora 'sig' será un vector con tres componentes (sig[4], sig[13], sig[14])
params2 = c("mu", "sig")

# Función para generar valores iniciales aleatorios para cada cadena en el Modelo 2
inits2 = function() {
  list("mu" = rnorm(3, 0.0, 100.0), # Medias iniciales aleatorias para cada grupo
       "prec" = rgamma(3, 1.0, 1.0)) # Tres precisiones iniciales aleatorias (una por grupo)
}

# Fijar semilla para reproducibilidad (opcional, para el segundo modelo)
set.seed(82)

# Compilar el Modelo 2
mod2 = jags.model(textConnection(mod2_string), data=data_jags, inits=inits2, n.chains=3)

# Fase de Burn-in
update(mod2, 1e3)

# Calcular el DIC para el Modelo 2 y guardarlo como 'dic2'
dic2 = dic.samples(mod2, n.iter=1e3)


# Calcular la diferencia entre los DICs
difference_dic = dic1 - dic2

# Imprimir los DICs y su diferencia
print("DIC del Modelo 1 (varianza única):")
print(dic1)
print("DIC del Modelo 2 (varianzas separadas):")
print(dic2)
print("Diferencia DIC1 - DIC2:")
print(difference_dic)