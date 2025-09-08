# --- PASO 1: Simular y Crear el Archivo "callers.csv" ---
# (Este bloque es idéntico y funcionó bien la primera vez)

set.seed(42)
n_total <- 224
n_group2 <- 24
n_other <- n_total - n_group2

isgroup2 <- c(rep(1, n_group2), rep(0, n_other))
days_active <- sample(10:90, n_total, replace = TRUE)
age <- sample(21:75, n_total, replace = TRUE)

calls_other <- round(rpois(n_other, lambda = days_active[isgroup2 == 0] * 0.15))
calls_group2 <- round(rpois(n_group2, lambda = days_active[isgroup2 == 1] * 0.45))
calls <- c(calls_group2, calls_other)

callers_data <- data.frame(calls, days_active, age, isgroup2)
write.csv(callers_data, "callers.csv", row.names = FALSE)

print("El archivo 'callers.csv' ha sido creado exitosamente.")

# --- PASO 2: Leer los Datos y Generar los Gráficos de la Pregunta ---

dat <- read.csv(file="callers.csv", header=TRUE)
dat$isgroup2 <- as.factor(dat$isgroup2)
dat$call_rate <- dat$calls / dat$days_active

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Gráfico: age vs. isgroup2
boxplot(age ~ isgroup2, data = dat,
        main = "1. Edad vs. Grupo",
        xlab = "Grupo (1=Grupo 2, 0=Otros)", ylab = "Edad",
        col = c("lightblue", "salmon"))

# 2. Gráfico: calls vs. isgroup2
boxplot(calls ~ isgroup2, data = dat,
        main = "2. Llamadas Totales vs. Grupo",
        xlab = "Grupo", ylab = "Nº de Llamadas Totales",
        col = c("lightblue", "salmon"))

# 3. Gráfico: calls / days_active vs. age
plot(call_rate ~ age, data = dat,
     main = "3. Tasa de Llamadas vs. Edad",
     xlab = "Edad", ylab = "Tasa de Llamadas (llamadas/día)",
     pch = 19, 
     col = "#0000FF80") # <-- ¡ESTA ES LA LÍNEA CORREGIDA!

# 4. Gráfico: calls / days_active vs. isgroup2 (EL MÁS ÚTIL)
boxplot(call_rate ~ isgroup2, data = dat,
        main = "4. Tasa de Llamadas vs. Grupo",
        xlab = "Grupo", ylab = "Tasa de Llamadas (llamadas/día)",
        col = c("lightblue", "salmon"))

par(mfrow = c(1, 1))
