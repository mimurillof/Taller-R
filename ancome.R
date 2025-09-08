library("rjags")
library("car") # Para el dataset Anscombe
data("Anscombe")

mod_string_B = " model {
  for (i in 1:n) {
    education[i] ~ dnorm(mu[i], prec)
    mu[i] = b0 + b[6]*income[i] + b[3]*young[i] + b[5]*income[i]*young[i] // 'urban' eliminado, interacción añadida
  }

  b0 ~ dnorm(0.0, 1.0/1.0e6)
  for (j in 1:3) { // Priors para b[6], b[3] y b[5] (interacción)
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
  sig2 = 1.0 / prec
  sig = sqrt(sig2)
} "

data_jags_B = list(
  education = Anscombe$education,
  income = Anscombe$income,
  young = Anscombe$young,
  n = nrow(Anscombe)
)

params = c("b0", "b", "sig")

set.seed(123) # Para reproducibilidad
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1000) # Fase de "burn-in" para que las cadenas se estabilicen
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5000) # Muestreo de la distribución posterior

# Calcular DIC para el modelo de Anscombe
# Se recomienda usar un n.iter alto como 100000 para fiabilidad
dic_anscombe = dic.samples(mod, n.iter=100000)
print(dic_anscombe)