# ===================================================================
# 1. DEFINICIÓN DE LAS FUNCIONES NECESARIAS
# [cite_start]Fuente: Apuntes de Clase [cite: 1073-1132]
# ===================================================================

# --- Función para actualizar mu ---
update_mu <- function(n, ybar, sig2, mu_0, sig2_0) {
  sig2_n <- 1.0 / (n / sig2 + 1.0 / sig2_0)
  mu_n <- sig2_n * (n * ybar / sig2 + mu_0 / sig2_0)
  rnorm(n = 1, mean = mu_n, sd = sqrt(sig2_n))
}

# --- Función para actualizar sigma^2 ---
update_sig2 <- function(n, y, mu, nu_0, beta_0) {
  nu_1 <- nu_0 + n / 2.0
  ss <- sum((y - mu)^2)
  beta_1 <- beta_0 + ss / 2.0
  out_gamma <- rgamma(n = 1, shape = nu_1, rate = beta_1)
  1.0 / out_gamma
}

# --- Función principal del Muestreador de Gibbs ---
gibbs <- function(y, n_iter, init, prior) {
  ybar <- mean(y)
  n <- length(y)
  mu_out <- numeric(n_iter)
  sig2_out <- numeric(n_iter)
  mu_now <- init$mu
  
  for (i in 1:n_iter) {
    sig2_now <- update_sig2(n = n, y = y, mu = mu_now,
                            nu_0 = prior$nu_0, beta_0 = prior$beta_0)
    mu_now <- update_mu(n = n, ybar = ybar, sig2 = sig2_now,
                        mu_0 = prior$mu_0, sig2_0 = prior$sig2_0)
    sig2_out[i] <- sig2_now
    mu_out[i] <- mu_now
  }
  
  cbind(mu = mu_out, sig2 = sig2_out)
}

# ===================================================================
# 2. EJECUCIÓN DEL ANÁLISIS PARA LA PREGUNTA
# ===================================================================

# --- Datos ---
y <- c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)

# --- Hiperparámetros de la Priori ---
prior <- list(
  mu_0 = 0.0,
  sig2_0 = 1.0,
  nu_0 = 1.0,
  beta_0 = 1.0
)

# --- Valores Iniciales ---
init <- list(
  mu = 0.0
)

# --- Ejecución del Muestreador ---
set.seed(53) # Para reproducibilidad
post <- gibbs(y = y, n_iter = 5000, init = init, prior = prior)

# --- Obtención del Resultado ---
resultado <- round(mean(post[, "mu"]), 2)

# Imprimir el resultado final
print(resultado)
# [1] -1.25