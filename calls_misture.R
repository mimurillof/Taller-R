# --------------------------------------------------------------------------
# Fase 1: Carga de librerías y datos
# --------------------------------------------------------------------------
library("rjags")

# Carga tus datos reales desde el archivo "callers.csv"
callers <- read.csv("callers.csv")

# --------------------------------------------------------------------------
# Fase 2: Definición del Modelo en JAGS (VERSIÓN CON EDAD CENTRADA)
# --------------------------------------------------------------------------
# CAMBIO CLAVE: El modelo ahora usa una variable 'age_cent' en lugar de 'age'.
mod_string <- "
model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois(mu[i])
        log(mu[i]) <- log(days_active[i]) + int + b_age*age_cent[i] + b_group*isgroup2[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_group ~ dnorm(0.0, 1.0/1e4)
}
"

# --------------------------------------------------------------------------
# Fase 3: Preparación y Ejecución del Modelo
# --------------------------------------------------------------------------
# CAMBIO CLAVE: Centramos la variable 'age' restándole su media.
mean_age <- mean(callers$age)
age_cent <- callers$age - mean_age

# Preparamos la lista de datos para JAGS con la edad ya centrada.
data_jags <- list(
  calls = callers$calls,
  age_cent = age_cent, # <-- Usamos la edad centrada
  isgroup2 = callers$isgroup2,
  days_active = callers$days_active
)

params <- c("int", "b_age", "b_group")

# Ejecución del modelo
set.seed(123)
mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1000)

mod_sim <- coda.samples(model = mod,
                        variable.names = params,
                        n.iter = 5000)

mod_csim <- as.matrix(do.call(rbind, mod_sim))

# --------------------------------------------------------------------------
# Fase 4: Simulación Predictiva Posterior para el Nuevo Cliente
# --------------------------------------------------------------------------
# Perfil del nuevo cliente
edad_nuevo <- 29
es_grupo2_nuevo <- 1
t <- 30

# CAMBIO CLAVE: Centramos la edad del nuevo cliente USANDO LA MISMA MEDIA del dataset original.
edad_nuevo_cent <- edad_nuevo - mean_age

# Usamos la edad centrada en el cálculo de la predicción
log_lam_pred <- mod_csim[, "int"] + 
  mod_csim[, "b_age"] * edad_nuevo_cent +
  mod_csim[, "b_group"] * es_grupo2_nuevo

# El resto del proceso es idéntico
lam_pred <- exp(log_lam_pred)
mu_pred_30dias <- lam_pred * t
n_sim <- nrow(mod_csim)
llamadas_predichas <- rpois(n_sim, lambda = mu_pred_30dias)
prob_al_menos_3 <- mean(llamadas_predichas >= 3)
respuesta_final <- round(prob_al_menos_3, 2)

# --------------------------------------------------------------------------
# Fase 5: Mostrar el Resultado
# --------------------------------------------------------------------------
print(paste("La probabilidad (modelo con edad centrada) de que llame al menos 3 veces es:", respuesta_final))