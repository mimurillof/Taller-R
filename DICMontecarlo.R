# 1. Cargar librerías necesarias [2]
library("rjags")
library("car") # Para el dataset Anscombe [2]

# 2. Cargar los datos [2]
data("Anscombe")

# 3. Definir el modelo en JAGS (modelo original) [2, 3]
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

# 4. Preparar los datos para JAGS [3]
data_jags = list(
  education = Anscombe$education,
  income = Anscombe$income,
  young = Anscombe$young,
  urban = Anscombe$urban,
  n = nrow(Anscombe)
)

# 5. Especificar los parámetros a monitorear [3]
params = c("b0", "b", "sig")

# 6. Configurar y ejecutar el modelo [4]
set.seed(123) # Para reproducibilidad [4]
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1000) # Fase de "burn-in" [4]
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5000) # Muestreo de la distribución posterior [4]

# 7. Combinar las cadenas en una sola matriz para facilitar el análisis [5]
mod_csim = do.call(rbind, mod_sim)

# 8. Extraer las muestras Monte Carlo para el coeficiente de 'income' (b[1])
income_coef_samples = mod_csim[, "b[1]"]

# 9. Calcular la probabilidad posterior de que el coeficiente sea positivo
prob_positive_income_coef = mean(income_coef_samples > 0.0)

# 10. Redondear la respuesta a dos decimales
rounded_prob = round(prob_positive_income_coef, 2)

# Imprimir la probabilidad redondeada
print(rounded_prob)