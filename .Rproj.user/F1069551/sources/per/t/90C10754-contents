# Paso 2: Planificar y Recopilar Datos (Versión Robusta y Definitiva)

# 1. Cargar la librería
library(quantmod)

# 2. Definir los parámetros
# Lista original de tickers que intentaremos descargar.
initial_tickers <- c("SPY", "XLK", "XLF", "XLP", "XLE")
start_date <- "2018-01-01"
end_date <- "2024-12-31"

# 3. Descargar datos en un entorno aislado y seguro
# Esto evita contaminar el espacio de trabajo global y facilita la gestión.
data_env <- new.env()
getSymbols(initial_tickers,
           env = data_env, # Descargar aquí, no en el entorno global
           from = start_date,
           to = end_date,
           src = "yahoo")

# 4. Verificar qué tickers se descargaron con éxito
# Creamos nuestra lista de tickers final basada en lo que realmente está en el entorno.
successful_tickers <- ls(data_env)

# Si no se descargó nada, detener y avisar.
if (length(successful_tickers) == 0) {
  stop("Error: No se pudo descargar ningún dato. Verifica tu conexión o los tickers.")
} else if (length(successful_tickers) < length(initial_tickers)) {
  # Avisar al usuario si faltó alguno.
  missing_tickers <- setdiff(initial_tickers, successful_tickers)
  warning(paste("No se pudieron descargar los siguientes tickers:", paste(missing_tickers, collapse = ", ")))
}

# 5. Procesar únicamente los datos descargados con éxito
# Extraemos los precios ajustados de los objetos dentro del entorno 'data_env'.
adjusted_prices <- lapply(successful_tickers, function(ticker) {
  Ad(data_env[[ticker]])
})

# Combinamos las series de precios en un único objeto.
merged_prices <- do.call(merge, adjusted_prices)

# Asignamos nombres a las columnas antes de calcular retornos
colnames(merged_prices) <- successful_tickers

# Calculamos los retornos logarítmicos para cada columna individualmente
log_returns_list <- lapply(1:ncol(merged_prices), function(i) {
  dailyReturn(merged_prices[, i], type = 'log')
})

# Combinamos los retornos en un solo objeto xts
log_returns <- do.call(merge, log_returns_list)
colnames(log_returns) <- successful_tickers
clean_log_returns <- na.omit(log_returns)

# 6. Verificar dimensiones antes de asignar nombres
cat("Dimensiones de clean_log_returns:", dim(clean_log_returns), "\n")
cat("Número de tickers exitosos:", length(successful_tickers), "\n")
returns_df <- as.data.frame(clean_log_returns)

# 7. Verificación final
cat("Descarga completada. Tickers procesados:", paste(successful_tickers, collapse = ", "), "\n\n")
cat("Nombres de columna del data frame final:\n")
print(colnames(returns_df))
cat("\nPrimeras filas de los datos listos:\n")
print(head(returns_df))

# Paso 3: Explorar los Datos (Ahora funcionará)

library(ggplot2)
library(tidyr)
library(dplyr)

# Paso 3: Explorar los Datos (Ahora funcionará)

library(ggplot2)
library(tidyr)
library(dplyr)

# Verificar que SPY esté en los datos
if ("SPY" %in% colnames(returns_df)) {
  # Crear datos en formato largo, manteniendo SPY como referencia
  returns_long <- returns_df %>%
    pivot_longer(cols = -SPY,
                 names_to = "Sector",
                 values_to = "Sector_Return")
  
  exploratory_plot <- ggplot(returns_long, aes(x = SPY, y = Sector_Return)) +
    geom_point(alpha = 0.3, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +
    facet_wrap(~ Sector, ncol = 2) +
    labs(
      title = "Relación de Retornos Diarios vs. Mercado (S&P 500)",
      subtitle = "Datos desde 2018 hasta 2024",
      x = "Retornos del Mercado (SPY)",
      y = "Retornos del Sector"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
  
  print(exploratory_plot)
} else {
  cat("Advertencia: SPY no está disponible en los datos. Mostrando gráfico alternativo...\n")
  
  # Crear un gráfico de series de tiempo alternativo
  returns_long_alt <- returns_df %>%
    mutate(Date = rownames(.)) %>%
    pivot_longer(cols = -Date,
                 names_to = "Ticker",
                 values_to = "Return") %>%
    mutate(Date = as.Date(Date))
  
  alt_plot <- ggplot(returns_long_alt, aes(x = Date, y = Return, color = Ticker)) +
    geom_line(alpha = 0.7) +
    facet_wrap(~ Ticker, ncol = 2, scales = "free_y") +
    labs(
      title = "Series de Retornos Diarios por ETF",
      subtitle = "Datos desde 2018 hasta 2024",
      x = "Fecha",
      y = "Retornos Logarítmicos"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
  
  print(alt_plot)
}

# Paso 4: Postular un Modelo (y Paso 5: Ajustar el Modelo)

# 1. Cargar las librerías necesarias para JAGS
# Si no las tienes, instálalas con install.packages(c("rjags", "coda"))
library(rjags)
library(coda)
library(dplyr) # Lo usamos para la preparación de datos

# 2. Preparar los datos para JAGS
# JAGS necesita los datos en un formato de lista.
# También necesitamos un índice numérico para los sectores.

# Asegúrate de tener el data frame 'returns_long' del paso anterior.
# Si no lo tienes, puedes volver a crearlo:
if (!exists("returns_long")) {
  library(tidyr)
  returns_long <- returns_df %>%
    pivot_longer(cols = -"SPY",
                 names_to = "Sector",
                 values_to = "Sector_Return")
}

# Crear el índice numérico para los sectores
returns_long <- returns_long %>%
  mutate(Sector_Index = as.numeric(as.factor(Sector)))

# Crear la lista de datos para JAGS
jags_data <- list(
  R_sector = returns_long$Sector_Return,
  R_market = returns_long$SPY,
  sector_idx = returns_long$Sector_Index,
  N = nrow(returns_long), # Número total de observaciones
  J = length(unique(returns_long$Sector)) # Número de sectores
)

# Guardar los nombres de los sectores para después
sector_names <- levels(as.factor(returns_long$Sector))

# 3. Especificar el modelo en el lenguaje de JAGS (como un string)
model_string <- "
model {
  # --- 1. Verosimilitud (Likelihood) ---
  # Se itera sobre cada observación de retorno
  for (i in 1:N) {
    # El retorno del sector sigue una distribución normal
    R_sector[i] ~ dnorm(mu[i], tau[sector_idx[i]])
    # El valor esperado (mu) es la ecuación del CAPM
    mu[i] <- alpha[sector_idx[i]] + beta[sector_idx[i]] * R_market[i]
  }

  # --- 2. Priors de Nivel de Sector (Parámetros para cada sector j) ---
  for (j in 1:J) {
    # Estructura jerárquica para Beta
    beta[j] ~ dnorm(mu_beta, tau_beta)
    # Estructura jerárquica para Alfa
    alpha[j] ~ dnorm(mu_alpha, tau_alpha)
    
    # Prior para la desviación estándar del error de cada sector
    sigma[j] ~ dunif(0, 100)
    # Convertir a precisión (requerido por JAGS)
    tau[j] <- 1 / pow(sigma[j], 2)
  }

  # --- 3. Hiperpriors (Priors para los parámetros del grupo) ---
  # Priors para la distribución de los Betas
  mu_beta ~ dnorm(1, 0.1)     # Centrado en 1 (beta de mercado) con baja precisión
  tau_beta ~ dgamma(0.1, 0.1) # Prior débilmente informativo para la precisión

  # Priors para la distribución de los Alfas
  mu_alpha ~ dnorm(0, 0.1)    # Centrado en 0 (sin retorno anormal) con baja precisión
  tau_alpha ~ dgamma(0.1, 0.1)# Prior débilmente informativo para la precisión
}
"

# 4. Configurar y ejecutar el modelo MCMC
# Parámetros que queremos monitorear y guardar
params_to_monitor <- c("alpha", "beta", "mu_alpha", "mu_beta", "sigma")

# Configuración de MCMC
n_chains <- 3
n_adapt <- 1000  # Fase de adaptación
n_burnin <- 2000 # Fase de descarte (burn-in)
n_iter <- 5000   # Iteraciones para muestrear de la posterior

# Crear el objeto del modelo JAGS
jags_model <- jags.model(textConnection(model_string),
                         data = jags_data,
                         n.chains = n_chains,
                         n.adapt = n_adapt)

# Realizar el burn-in
update(jags_model, n.iter = n_burnin)

# Muestrear de la distribución posterior
posterior_samples <- coda.samples(jags_model,
                                  variable.names = params_to_monitor,
                                  n.iter = n_iter)

# 5. Revisar los resultados (Iniciando el Paso 6: Comprobar el Modelo)
cat("¡Modelo ajustado con éxito!\n\n")

# --- Código Corregido para un Resumen Limpio ---

# Extraer las estadísticas y cuantiles del resumen
summary_stats <- summary(posterior_samples)
stats <- as.data.frame(summary_stats$statistics)
quants <- as.data.frame(summary_stats$quantiles)

# Crear un mapeo de índice a nombre de sector (asegúrate que el orden es correcto)
# El orden es alfabético por defecto cuando 'as.factor' se aplica.
sector_names <- levels(as.factor(returns_long$Sector))
index_map <- data.frame(Index = 1:length(sector_names), Sector = sector_names)
print("Mapeo de Índice a Sector:")
print(index_map)

# Reemplazar los índices en los nombres de fila (ej. "beta[1]") con los nombres de sector
# Hacemos esto para cada tipo de parámetro
for(i in 1:nrow(index_map)) {
  rownames(stats) <- gsub(paste0("\\[", i, "\\]"), paste0(" [", index_map$Sector[i], "]"), rownames(stats))
  rownames(quants) <- gsub(paste0("\\[", i, "\\]"), paste0(" [", index_map$Sector[i], "]"), rownames(quants))
}

# Imprimir un resumen final y limpio
cat("\n--- Resumen de Parámetros con Nombres Corregidos ---\n")
print(cbind(stats, quants))

# Genera los "trace plots" y gráficos de densidad
plot(posterior_samples)

# =============================================================================
# Paso 6: Comprobar el Modelo - Análisis de Convergencia y Residuos
# =============================================================================

cat("\n=== PASO 6: COMPROBACIÓN DEL MODELO ===\n")

# A. Diagnóstico de Convergencia MCMC
cat("\n--- A. Diagnóstico de Convergencia ---\n")
cat("Los gráficos de traza y densidad han sido generados arriba.\n")
cat("Verificar visualmente que las cadenas estén bien mezcladas.\n")
cat("Las cadenas deben verse como 'orugas peludas' sin tendencias.\n\n")

# Calcular estadísticos de diagnóstico de convergencia (Rhat)
gelman_diag <- gelman.diag(posterior_samples)
cat("Diagnóstico de Gelman-Rubin (Rhat):\n")
print(gelman_diag)
cat("\nRhat cercano a 1.0 indica convergencia adecuada.\n")

# B. Análisis de Residuos
cat("\n--- B. Análisis de Residuos ---\n")

# Obtener las medias posteriores de alfa y beta para cada sector
alpha_mean <- stats[grep("alpha \\[", rownames(stats)), "Mean"]
beta_mean <- stats[grep("beta \\[", rownames(stats)), "Mean"]

# Verificar que tenemos los parámetros correctos
cat("Número de alphas encontrados:", length(alpha_mean), "\n")
cat("Número de betas encontrados:", length(beta_mean), "\n")
cat("Número de sectores únicos:", length(unique(returns_long$Sector_Index)), "\n")

# Añadir las predicciones y residuos al dataframe
returns_long <- returns_long %>%
  group_by(Sector_Index) %>%
  mutate(
    # El retorno predicho por el modelo (usando las medias posteriores)
    Predicted_Return = alpha_mean[Sector_Index] + beta_mean[Sector_Index] * SPY,
    # El residuo es la diferencia entre lo real y lo predicho
    Residual = Sector_Return - Predicted_Return
  ) %>%
  ungroup()

# Gráfico de Residuos vs. Valores Ajustados
library(ggplot2)
residual_plot <- ggplot(returns_long, aes(x = Predicted_Return, y = Residual)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~ Sector) +
  labs(
    title = "Análisis de Residuos por Sector",
    subtitle = "Verificación de supuestos del modelo",
    x = "Retorno Predicho (Valores Ajustados)",
    y = "Residuos"
  ) +
  theme_minimal()

print(residual_plot)

# Gráfico Q-Q para verificar la normalidad de los residuos
qq_plot <- ggplot(returns_long, aes(sample = Residual)) +
  stat_qq(alpha = 0.1) +
  stat_qq_line(color = "red") +
  facet_wrap(~ Sector) +
  labs(
    title = "Gráfico Q-Q de los Residuos por Sector",
    subtitle = "Verificación de normalidad de residuos",
    x = "Cuantiles Teóricos (Normal)",
    y = "Cuantiles de la Muestra"
  ) +
  theme_minimal()

print(qq_plot)

cat("\nInterpretación del análisis de residuos:\n")
cat("1. Gráfico de residuos vs. ajustados: Debe mostrar nube aleatoria centrada en cero\n")
cat("2. Gráfico Q-Q: Los puntos deben seguir la línea roja para indicar normalidad\n\n")

# =============================================================================
# Paso 8: Utilizar el Modelo - Interpretación de Resultados
# =============================================================================

cat("\n=== PASO 8: INTERPRETACIÓN DE RESULTADOS ===\n")

# A. Interpretación del Riesgo Sistémico (Beta)
cat("\n--- A. Interpretación del Riesgo Sistémico (Beta) ---\n")

# Extraer información de Beta con intervalos de credibilidad
beta_results <- cbind(stats[grep("beta \\[", rownames(stats)), c("Mean", "SD")],
                      quants[grep("beta \\[", rownames(quants)), c("2.5%", "97.5%")])

cat("Interpretación de los valores Beta:\n")
cat("• β > 1: Sector agresivo (se mueve más que el mercado)\n")
cat("• β < 1: Sector defensivo (se mueve menos que el mercado)\n")
cat("• β ≈ 1: Sector neutral (se mueve en línea con el mercado)\n\n")

print("Resultados de Beta por Sector:")
print(beta_results)

# Interpretación automática de cada Beta
cat("\nInterpretación de cada sector:\n")
for(i in 1:nrow(beta_results)) {
  sector_name <- gsub(".*\\[(.+)\\].*", "\\1", rownames(beta_results)[i])
  beta_mean <- beta_results[i, "Mean"]
  ci_lower <- beta_results[i, "2.5%"]
  ci_upper <- beta_results[i, "97.5%"]
  
  if(ci_lower > 1) {
    risk_type <- "CLARAMENTE AGRESIVO"
  } else if(ci_upper < 1) {
    risk_type <- "CLARAMENTE DEFENSIVO"
  } else if(beta_mean > 1) {
    risk_type <- "Ligeramente agresivo"
  } else {
    risk_type <- "Ligeramente defensivo"
  }
  
  cat(sprintf("• %s: β = %.3f [IC 95%%: %.3f - %.3f] - %s\n", 
              sector_name, beta_mean, ci_lower, ci_upper, risk_type))
}

# B. Interpretación del Rendimiento Anormal (Alfa)
cat("\n--- B. Interpretación del Rendimiento Anormal (Alfa) ---\n")

# Extraer información de Alfa con intervalos de credibilidad
alpha_results <- cbind(stats[grep("alpha \\[", rownames(stats)), c("Mean", "SD")],
                       quants[grep("alpha \\[", rownames(quants)), c("2.5%", "97.5%")])

cat("Interpretación de los valores Alfa:\n")
cat("• α > 0: Rendimiento superior al esperado por el riesgo\n")
cat("• α < 0: Rendimiento inferior al esperado por el riesgo\n")
cat("• α ≈ 0: Rendimiento en línea con el riesgo (no hay alpha anormal)\n\n")

print("Resultados de Alfa por Sector:")
print(alpha_results)

# Verificar si algún alfa es estadísticamente significativo
cat("\nEvidencia de rendimientos anormales:\n")
significant_alphas <- 0
for(i in 1:nrow(alpha_results)) {
  sector_name <- gsub(".*\\[(.+)\\].*", "\\1", rownames(alpha_results)[i])
  alpha_mean <- alpha_results[i, "Mean"]
  ci_lower <- alpha_results[i, "2.5%"]
  ci_upper <- alpha_results[i, "97.5%"]
  
  if(ci_lower > 0) {
    cat(sprintf("• %s: Evidencia FUERTE de rendimiento superior (α > 0)\n", sector_name))
    significant_alphas <- significant_alphas + 1
  } else if(ci_upper < 0) {
    cat(sprintf("• %s: Evidencia FUERTE de rendimiento inferior (α < 0)\n", sector_name))
    significant_alphas <- significant_alphas + 1
  } else {
    cat(sprintf("• %s: Sin evidencia de rendimiento anormal (IC contiene 0)\n", sector_name))
  }
}

if(significant_alphas == 0) {
  cat("\nCONCLUSIÓN: No se encontró evidencia estadísticamente creíble de que\n")
  cat("alguno de los sectores generara rendimientos anormales durante el periodo,\n")
  cat("una vez ajustado por su riesgo de mercado.\n")
}

# C. Resumen Ejecutivo
cat("\n--- C. Resumen Ejecutivo ---\n")
cat("HALLAZGOS PRINCIPALES:\n")
cat("1. Riesgo por Sector:\n")

# Ordenar sectores por Beta
beta_ordered <- beta_results[order(beta_results[,"Mean"], decreasing = TRUE), ]
for(i in 1:nrow(beta_ordered)) {
  sector_name <- gsub(".*\\[(.+)\\].*", "\\1", rownames(beta_ordered)[i])
  beta_mean <- beta_ordered[i, "Mean"]
  cat(sprintf("   %d. %s (β = %.3f)\n", i, sector_name, beta_mean))
}

cat("\n2. Rendimientos Anormales:\n")
if(significant_alphas == 0) {
  cat("   • Ningún sector mostró evidencia de alpha significativo\n")
  cat("   • Los retornos son consistentes con el modelo CAPM\n")
} else {
  cat(sprintf("   • %d sector(es) mostraron evidencia de alpha significativo\n", significant_alphas))
}

cat("\n3. Implicaciones para la Inversión:\n")
cat("   • Los sectores más agresivos ofrecen mayor potencial de ganancia pero también mayor riesgo\n")
cat("   • Los sectores defensivos pueden servir como refugio en mercados volátiles\n")
cat("   • La ausencia de alphas significativos sugiere eficiencia de mercado\n\n")

# =============================================================================
# Paso 7: Modelo Alternativo - No-Pooling (Regresiones Independientes)
# =============================================================================

cat("\n=== PASO 7: MODELO ALTERNATIVO (NO-POOLING) ===\n")
cat("Ajustando regresiones bayesianas independientes para cada sector...\n\n")

# Especificar el modelo no-pooling en JAGS
no_pooling_model_string <- "
model {
  # --- 1. Verosimilitud (Likelihood) ---
  for (i in 1:N) {
    R_sector[i] ~ dnorm(mu[i], tau[sector_idx[i]])
    mu[i] <- alpha[sector_idx[i]] + beta[sector_idx[i]] * R_market[i]
  }

  # --- 2. Priors Independientes para cada Sector ---
  for (j in 1:J) {
    # Priors independientes (no jerárquicos) para cada sector
    beta[j] ~ dnorm(1, 0.1)     # Prior centrado en 1, baja precisión
    alpha[j] ~ dnorm(0, 0.1)    # Prior centrado en 0, baja precisión
    
    # Prior para la desviación estándar del error
    sigma[j] ~ dunif(0, 100)
    tau[j] <- 1 / pow(sigma[j], 2)
  }
}
"

# Ajustar el modelo no-pooling
cat("Configurando modelo no-pooling...\n")
no_pooling_model <- jags.model(textConnection(no_pooling_model_string),
                               data = jags_data,
                               n.chains = n_chains,
                               n.adapt = n_adapt)

# Burn-in
update(no_pooling_model, n.iter = n_burnin)

# Muestrear
no_pooling_samples <- coda.samples(no_pooling_model,
                                   variable.names = c("alpha", "beta", "sigma"),
                                   n.iter = n_iter)

# Procesar resultados del modelo no-pooling
no_pooling_summary <- summary(no_pooling_samples)
no_pooling_stats <- as.data.frame(no_pooling_summary$statistics)
no_pooling_quants <- as.data.frame(no_pooling_summary$quantiles)

# Aplicar nombres correctos
for(i in 1:nrow(index_map)) {
  rownames(no_pooling_stats) <- gsub(paste0("\\[", i, "\\]"), 
                                     paste0(" [", index_map$Sector[i], "]"), 
                                     rownames(no_pooling_stats))
  rownames(no_pooling_quants) <- gsub(paste0("\\[", i, "\\]"), 
                                      paste0(" [", index_map$Sector[i], "]"), 
                                      rownames(no_pooling_quants))
}

cat("\n--- Resultados del Modelo No-Pooling ---\n")
print(cbind(no_pooling_stats, no_pooling_quants))

# =============================================================================
# Comparación de Modelos: Jerárquico vs No-Pooling
# =============================================================================

cat("\n=== COMPARACIÓN DE MODELOS ===\n")

# Extraer Betas de ambos modelos para comparación
hierarchical_betas <- beta_results
no_pooling_betas <- cbind(no_pooling_stats[grep("beta \\[", rownames(no_pooling_stats)), c("Mean", "SD")],
                          no_pooling_quants[grep("beta \\[", rownames(no_pooling_quants)), c("2.5%", "97.5%")])

# Crear tabla comparativa
comparison_table <- data.frame(
  Sector = gsub(".*\\[(.+)\\].*", "\\1", rownames(hierarchical_betas)),
  Hierarchical_Beta = hierarchical_betas[, "Mean"],
  Hierarchical_SD = hierarchical_betas[, "SD"],
  Hierarchical_CI_Width = hierarchical_betas[, "97.5%"] - hierarchical_betas[, "2.5%"],
  NoPooling_Beta = no_pooling_betas[, "Mean"],
  NoPooling_SD = no_pooling_betas[, "SD"],
  NoPooling_CI_Width = no_pooling_betas[, "97.5%"] - no_pooling_betas[, "2.5%"]
)

cat("\n--- Comparación de Estimaciones Beta ---\n")
print(comparison_table)

# Calcular mejoras en precisión
cat("\n--- Mejoras en Precisión (Modelo Jerárquico vs No-Pooling) ---\n")
for(i in 1:nrow(comparison_table)) {
  sector <- comparison_table$Sector[i]
  precision_improvement <- (comparison_table$NoPooling_CI_Width[i] - comparison_table$Hierarchical_CI_Width[i]) / comparison_table$NoPooling_CI_Width[i] * 100
  
  cat(sprintf("%s: %.1f%% reducción en ancho del IC 95%%\n", sector, precision_improvement))
}

# Calcular DIC para comparación de modelos
cat("\n--- Comparación de Ajuste del Modelo (DIC) ---\n")
hierarchical_dic <- dic.samples(jags_model, n.iter = 1000)
no_pooling_dic <- dic.samples(no_pooling_model, n.iter = 1000)

cat("DIC Modelo Jerárquico:", hierarchical_dic[[1]], "\n")
cat("DIC Modelo No-Pooling:", no_pooling_dic[[1]], "\n")
cat("Diferencia (Jerárquico - No-Pooling):", hierarchical_dic[[1]] - no_pooling_dic[[1]], "\n")
cat("(Un DIC menor indica mejor ajuste)\n\n")

cat("CONCLUSIÓN DE LA COMPARACIÓN:\n")
cat("1. El modelo jerárquico típicamente produce estimaciones más precisas\n")
cat("2. Los intervalos de credibilidad son más estrechos en el modelo jerárquico\n")
cat("3. El modelo jerárquico aprovecha la información compartida entre sectores\n")
cat("4. El DIC puede indicar cuál modelo tiene mejor balance ajuste-complejidad\n\n")

cat("¡ANÁLISIS COMPLETO! El proyecto está listo para el informe final.\n")