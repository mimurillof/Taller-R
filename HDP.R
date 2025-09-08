# Cargar las librerías necesarias
library(rjags)
library(coda)

# --- 1. Cargar y preparar los datos ---
data("PlantGrowth")
data_jags <- list(y = PlantGrowth$weight,
                  grp = as.numeric(PlantGrowth$group),
                  n_groups = 3,
                  n_total = length(PlantGrowth$weight))

# --- 2. Definir y ejecutar el Modelo ORIGINAL (Varianza Común) ---
# [cite_start]Usar la misma semilla de la lección para obtener resultados consistentes [cite: 1054]
set.seed(82) 

mod_string <- "
model {
  for (i in 1:n_total) {
    y[i] ~ dnorm(mu[grp[i]], prec)
  }
  for (j in 1:n_groups) {
    mu[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  prec ~ dgamma(5/2.0, 5*1.0/2.0)
}"

# Compilar el modelo
mod <- jags.model(textConnection(mod_string),
                  data = data_jags,
                  n.chains = 3)

# Fase de Burn-in
update(mod, 1000)

# Muestreo de la posterior
mod_sim <- coda.samples(model = mod,
                        variable.names = c("mu"), # Solo necesitamos monitorear mu
                        n.iter = 5000)

# --- 3. Calcular la diferencia y el intervalo HPD ---

# [cite_start]Combinar las cadenas MCMC en una sola matriz para facilitar el cálculo [cite: 1071]
mod_csim <- as.mcmc(do.call(rbind, mod_sim))

# Crear un vector con la diferencia (mu[3] - mu[1]) para cada muestra de la posterior
diff_mu <- mod_csim[, "mu[3]"] - mod_csim[, "mu[1]"]

# [cite_start]Calcular el intervalo HPD del 95% para este vector de diferencias [cite: 1165, 1175]
# La función HPDinterval requiere un objeto de tipo MCMC
hpd_interval <- HPDinterval(as.mcmc(diff_mu), prob = 0.95)

# Imprimir el resultado
print(hpd_interval)