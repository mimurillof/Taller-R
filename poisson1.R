# --- 1. Carga de Librerías y Datos ---
# Asegúrate de tener instalados los paquetes "rjags" y "COUNT"
# install.packages("rjags")
# install.packages("COUNT")

library("rjags")
library("COUNT")
data("badhealth")

# Preparar los datos para JAGS
data_jags <- as.list(badhealth)
set.seed(102) # Para reproducibilidad

# --- 2. Modelo Original (con Término de Interacción) ---

# Definición del modelo original en JAGS
mod_string_original <- "
model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    # Priors
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
}
"

# Parámetros a monitorear
params_original <- c("int", "b_badh", "b_age", "b_intx")

# Compilar y ejecutar el modelo original
mod_original <- jags.model(textConnection(mod_string_original), data = data_jags, n.chains = 3)
update(mod_original, 1e3) # Burn-in

# Calcular el DIC para el modelo original
# Nota: Usamos 5000 iteraciones para ser consistentes con las lecciones
dic_original <- dic.samples(mod_original, n.iter = 5e3)

print("--- RESULTADOS DEL MODELO ORIGINAL ---")
print(dic_original)


# --- 3. Modelo Aditivo (sin Término de Interacción) ---

# Definición del modelo aditivo (más simple)
mod_string_simple <- "
model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    # Priors
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
}
"

# Parámetros a monitorear (sin b_intx)
params_simple <- c("int", "b_badh", "b_age")

# Compilar y ejecutar el modelo simple
mod_simple <- jags.model(textConnection(mod_string_simple), data = data_jags, n.chains = 3)
update(mod_simple, 1e3) # Burn-in

# Calcular el DIC para el modelo simple
dic_simple <- dic.samples(mod_simple, n.iter = 5e3)

print("--- RESULTADOS DEL MODELO SIMPLE (ADITIVO) ---")
print(dic_simple)

# --- 4. Conclusión ---
# Compara la "Mean deviance" (Dbar) o la penalización (pD) y el DIC total.
# El modelo con el DIC más bajo es el preferido.