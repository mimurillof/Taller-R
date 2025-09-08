# 1. Cargar librería
library(rjags)

# 2. Cargar y preparar los datos
data("warpbreaks")
data_jags_q5 <- list(
  y = log(warpbreaks$breaks),
  woolGrp = as.numeric(warpbreaks$wool),
  tensGrp = as.numeric(warpbreaks$tension)
)

# 3. Definir el modelo JAGS con varianza separada por grupo
mod_string_q5 <- "
model {
  for(i in 1:length(y)) {
    # La verosimilitud ahora usa una precisión específica del grupo
    y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec[woolGrp[i], tensGrp[i]])
  }

  # Priors para las medias de cada grupo (medias de celda)
  for (j in 1:2) { # 2 niveles de 'wool'
    for (k in 1:3) { # 3 niveles de 'tension'
      mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
    }
  }

  # Priors para las precisiones (una por cada grupo)
  for (j in 1:2) {
    for (k in 1:3) {
      # Prior Gamma(0.5, 0.5) para cada precisión
      prec[j,k] ~ dgamma(0.5, 0.5)
      sig[j,k] = sqrt(1.0 / prec[j,k]) # Cantidad derivada opcional
    }
  }
}
"

# 4. Correr el modelo y calcular el DIC
set.seed(42) # Para reproducibilidad
mod_q5 <- jags.model(textConnection(mod_string_q5), data = data_jags_q5, n.chains = 3)
update(mod_q5, 5000) # Un burn-in más largo para asegurar convergencia
dic_q5 <- dic.samples(mod_q5, n.iter = 10000) # Más iteraciones para un DIC estable

# 5. Analizar el resultado
print(dic_q5)