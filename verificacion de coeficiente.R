# --- PASO 1: Cargar/Crear los Datos ---
# Asegúrate de que el archivo "callers.csv" exista en tu directorio de trabajo.
# Si no, ejecuta el código de la pregunta 5 para crearlo.

# Leer los datos
dat <- read.csv(file="callers.csv", header=TRUE)

# --- PASO 2: Ajustar el Modelo JAGS con los Priors Especificados ---
library("rjags")

# Especificación del modelo
# El prior N(0, 10^2) se traduce en JAGS como dnorm(0.0, 1.0/100)
# ya que JAGS utiliza la precisión (1/varianza) en lugar de la varianza.
mod_string_priors <- "
model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois( days_active[i] * lam[i] )
        log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
    }
    
    # Priors N(0, 10^2) para todos los coeficientes
    b0 ~ dnorm(0.0, 1.0/100)
    b[1] ~ dnorm(0.0, 1.0/100)
    b[2] ~ dnorm(0.0, 1.0/100)
}
"

# Preparar los datos para JAGS
data_jags <- list(
  calls = dat$calls,
  days_active = dat$days_active,
  age = dat$age,
  isgroup2 = dat$isgroup2 # Numérico, 0 o 1
)

# Parámetros a monitorear
params <- c("b0", "b")

# Compilar y ejecutar el modelo
set.seed(102) # Para reproducibilidad
mod <- jags.model(textConnection(mod_string_priors), data = data_jags, n.chains = 3)
update(mod, 1e3) # Burn-in
mod_sim <- coda.samples(model = mod, variable.names = params, n.iter = 5e3)

# --- PASO 3: Calcular la Probabilidad a Posteriori ---

# Convertir las simulaciones a una matriz para facilitar el acceso
mod_csim <- as.mcmc(do.call(rbind, mod_sim))

# El coeficiente para isgroup2 es la columna "b[2]"
b2_samples <- mod_csim[, "b[2]"]

# Calcular la proporción de muestras donde b[2] es mayor que 0
prob_b2_positive <- mean(b2_samples > 0)

# Redondear a dos decimales
prob_rounded <- round(prob_b2_positive, 2)

# Imprimir el resultado
print(paste("La probabilidad a posteriori de que beta_2 sea mayor que 0 es:", prob_rounded))

# --- (Opcional) Verificación Adicional ---
# Puedes ver un resumen de la distribución posterior para b[2]
# y un gráfico de densidad para confirmar visualmente el resultado.
print("Resumen de la distribución posterior para b[2]:")
summary(b2_samples)

plot(density(b2_samples), main="Distribución Posterior de beta_2 (isgroup2)",
     xlab="Valor del coeficiente", ylab="Densidad")
abline(v=0, col="red", lty=2) # Línea en cero para referencia visual
