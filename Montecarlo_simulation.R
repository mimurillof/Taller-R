# Paso 1: Configurar la simulación
set.seed(32)  # Para asegurar que los resultados sean reproducibles [7, 10]
m <- 100000    # Usamos un número grande de muestras para una buena aproximación [9, 11]
shape1_alpha <- 5 # Parámetro de forma 'a' o 'alfa' de la distribución Beta
shape2_beta <- 3  # Parámetro de forma 'b' o 'beta' de la distribución Beta

# Simular 'm' muestras de la distribución posterior Beta(5, 3) para theta
# La función para generar muestras de una distribución Beta en R es rbeta() [12, 13]
theta_samples <- rbeta(m, shape1 = shape1_alpha, shape2 = shape2_beta)

# Paso 2: Transformar cada muestra de theta a "probabilidades de éxito"
# Aplicamos la función h(theta) = theta / (1 - theta) a cada elemento del vector [2]
odds_samples <- theta_samples / (1 - theta_samples)

# Paso 3: Calcular la media de las muestras transformadas para aproximar el valor esperado
# Usamos la función mean() como se demostró en las fuentes [11, 14]
posterior_mean_odds <- mean(odds_samples)

# Imprimir el resultado
print(posterior_mean_odds)

# Las variables 'm' y 'odds_samples' ya fueron creadas en la pregunta anterior.
# m <- 100000
# odds_samples <- theta_samples / (1 - theta_samples)

# Paso 3: Aplicar la función indicadora.
# Creamos un vector booleano (TRUE/FALSE) que es VERDADERO si la probabilidad
# de éxito es mayor que 1.0 y FALSO en caso contrario.
indicator_odds_greater_than_1 <- (odds_samples > 1.0)

# Podemos ver algunos ejemplos de los resultados
# head(indicator_odds_greater_than_1)

# Paso 4: Calcular la media del vector indicador para aproximar la probabilidad.
# R automáticamente convierte TRUE a 1 y FALSE a 0 al calcular la media.
posterior_prob_odds_greater_than_1 <- mean(indicator_odds_greater_than_1)

# Imprimir el resultado
print(posterior_prob_odds_greater_than_1)

# Paso 1: Configurar la simulación
set.seed(32)  # Para asegurar que los resultados sean reproducibles [1, 6]
m <- 100000    # Usamos un número grande de muestras para una buena aproximación [7]

# Generar 'm' muestras de la distribución Normal estándar (media=0, sd=1)
# La función para esto en R es rnorm()
normal_samples <- rnorm(m, mean = 0, sd = 1)

# Paso 2 y 3: Aproximar el cuantil 0.3 utilizando la muestra Monte Carlo
# Usamos la función quantile() como se indica en las fuentes [3, 4, 8]
# Le pasamos el vector de muestras y la probabilidad deseada
mc_quantile_03 <- quantile(normal_samples, probs = 0.3)

# Imprimir el resultado de la aproximación Monte Carlo
print(paste("Aproximación Monte Carlo del cuantil 0.3:", round(mc_quantile_03, 4)))

# Paso 4: Comprobar la respuesta con el valor teórico exacto
# La función qnorm() calcula el cuantil teórico de una distribución Normal [3]
true_quantile_03 <- qnorm(0.3, mean = 0, sd = 1)

# Imprimir el valor verdadero para comparar
print(paste("Valor teórico verdadero del cuantil 0.3:", round(true_quantile_03, 4)))