# Cargar la librería necesaria para interactuar con JAGS
library("rjags")

# --- 1. Carga de tus Datos ---
dat <- read.csv("pctgrowth.csv", header = TRUE)

# Verificamos que los datos se hayan cargado correctamente
print(head(dat))

# ***** CAMBIO 1: Definir el número de observaciones *****
# Creamos una variable 'n' para guardar el número de filas (observaciones)
n <- nrow(dat)


# --- 2. Especificación del Modelo Jerárquico en JAGS ---
mod_string <- "
model {
    # Nivel 1: Likelihood (Datos)
    # ***** CAMBIO 2: Usar 'n' en lugar de 'length(y)' *****
    for (i in 1:n) {
        y[i] ~ dnorm(theta[grp[i]], prec_y)
    }

    # Nivel 2: Parámetros de Grupo (Industrias)
    for (g in 1:max(grp)) {
        theta[g] ~ dnorm(mu, prec_theta)
    }

    # Nivel 3: Hiperpriores
    mu ~ dnorm(0.0, 1.0/1.0e6)
    
    prec_theta ~ dgamma(1.0/2.0, 3.0/2.0)
    tau <- sqrt(1.0 / prec_theta)

    prec_y ~ dgamma(2.0/2.0, 2.0/2.0)
    sigma <- sqrt(1.0 / prec_y)
}
"

# --- 3. Preparación y Ejecución del Modelo ---
# Preparamos la lista de datos para JAGS, incluyendo ahora 'n'
data_jags <- list(y = dat$pctgrowth, grp = dat$grp, n = n)

# Parámetros que queremos monitorear
params <- c("theta", "mu", "tau", "sigma")

# Inicializar y compilar el modelo
mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)

# Ejecutar fase de calentamiento (burn-in)
update(mod, 1000)

# Obtener las muestras de la distribución posterior
mod_sim <- coda.samples(model = mod, variable.names = params, n.iter = 5000)

# --- 4. Ver los Resultados ---
# Resumen de las distribuciones posteriores para los parámetros
print(summary(mod_sim))