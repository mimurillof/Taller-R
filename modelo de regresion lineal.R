# 1. Cargar librerías necesarias
library("rjags")
library("car") # Para el dataset Anscombe

# 2. Cargar los datos
data("Anscombe")

# 3. Definir el modelo en una cadena de texto (como en la pregunta)
mod_string = " model {
    for (i in 1:n) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

# 4. Preparar los datos para JAGS
# Es mejor ser explícito y añadir 'n' para el tamaño de la muestra
data_jags = list(
  education = Anscombe$education,
  income = Anscombe$income,
  young = Anscombe$young,
  urban = Anscombe$urban,
  n = nrow(Anscombe)
)

# 5. Especificar los parámetros a monitorear
params = c("b0", "b", "sig")

# 6. Configurar y ejecutar el modelo
# Usaremos 3 cadenas para poder verificar la convergencia entre ellas
set.seed(123) # Para reproducibilidad
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1000) # Fase de "burn-in" para que las cadenas se estabilicen

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5000) # Muestreo de la distribución posterior

# 7. Realizar los diagnósticos
# Gráficos de traza y densidad (inspección visual)
plot(mod_sim)

# Diagnóstico de Gelman-Rubin (convergencia)
# Valores cercanos a 1.0 indican buena convergencia
gelman.diag(mod_sim)

# Gráficos de autocorrelación
# Si las barras bajan lentamente, hay alta autocorrelación
autocorr.plot(mod_sim)