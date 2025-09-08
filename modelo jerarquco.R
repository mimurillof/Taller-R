# --- 5. Comparación de Estimaciones ---

# a) Calcular las medias simples (el enfoque "ANOVA")
means_anova <- tapply(dat$y, INDEX = dat$grp, FUN = mean)

# b) Extraer las medias posteriores del modelo jerárquico
# Convertimos la salida de JAGS en una matriz para facilitar los cálculos
mod_csim <- as.matrix(do.call(rbind, mod_sim))

# Calculamos la media posterior para cada parámetro theta
means_theta <- colMeans(mod_csim)[grep("theta", colnames(mod_csim))]

# Extraemos la media global 'mu' estimada por el modelo
global_mean_mu <- colMeans(mod_csim)["mu"]


# c) Comparación numérica: Varianza de las estimaciones
cat("--- Comparación Numérica ---\n")
cat("Varianza de las medias simples (ANOVA):", var(means_anova), "\n")
cat("Varianza de las medias jerárquicas:", var(means_theta), "\n\n")


# d) Comparación visual
cat("--- Generando Gráfico Comparativo ---\n")
# Graficar las medias simples en azul
plot(means_anova, 
     main = "Efecto de Contracción (Shrinkage)", 
     xlab = "Industria (Grupo)", 
     ylab = "Crecimiento Medio Estimado",
     pch = 16, cex = 1.5, col = "royalblue",
     ylim = range(c(means_anova, means_theta))) # Asegura que todos los puntos quepan

# Añadir las medias jerárquicas en rojo
points(means_theta, pch = 17, cex = 1.5, col = "firebrick")

# Añadir la media global (mu) como una línea de referencia
abline(h = global_mean_mu, col = "firebrick", lty = 2, lwd = 2)

# Añadir flechas para visualizar el efecto de "contracción"
arrows(x0 = 1:length(means_anova), y0 = means_anova, 
       x1 = 1:length(means_theta), y1 = means_theta, 
       col = "grey50", length = 0.1)

# Añadir leyenda
legend("bottomleft", 
       legend = c("Media Simple (ANOVA)", "Media Jerárquica", "Media Global (mu)"), 
       col = c("royalblue", "firebrick", "firebrick"), 
       pch = c(16, 17, NA), 
       lty = c(NA, NA, 2),
       lwd = c(NA, NA, 2),
       bty = "n")