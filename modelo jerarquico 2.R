# --- 1. Cargar Librerías y Preparar Datos ---
library("MASS")
library("rjags")

# Cargar el conjunto de datos OME
data("OME")

# Limpiar datos y reformatear variables
dat <- subset(OME, OME != "N/A")
dat$OME <- factor(dat$OME) # re-etiquetar OME
dat$ID <- as.numeric(factor(dat$ID)) # re-etiquetar ID de 1 a 63

# Crear la matriz de covariables (X) para facilitar el paso de datos a JAGS
mod_glm <- glm(Correct / Trials ~ Age + OME + Loud + Noise, data = dat, weights = Trials, family = "binomial")
X <- model.matrix(mod_glm)[, -1]


# --- 2. Especificación del NUEVO Modelo Jerárquico en JAGS ---
# Este es el modelo de la pregunta 2, que reemplaza al modelo original
mod_string_hierarchical <- "
model {
    # Nivel 1: Likelihood
    for (i in 1:length(y)) {
        y[i] ~ dbin(phi[i], n[i])
        # El intercepto b0 se reemplaza por a[ID[i]]
        logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
    }
    
    # Nivel 2: Priors para los interceptos de cada niño
    for (j in 1:n_kids) {
        a[j] ~ dnorm(mu, prec_a) # n_kids será 63
    }
    
    # Nivel 3: Hiperpriores para los parámetros de Nivel 2
    mu ~ dnorm(0.0, 1.0/10.0^2)
    prec_a ~ dgamma(0.5, 0.5) # Corresponde a tau^2 ~ IG(1/2, 1/2)
    tau <- sqrt(1.0 / prec_a) # Parámetro derivado para la desviación estándar
    
    # Priors para los coeficientes fijos (betas)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1.0/4.0^2)
    }
}
"

# --- 3. Preparación y Ejecución del Modelo ---
# Crear la lista de datos para JAGS
data_jags <- as.list(as.data.frame(X))
data_jags$y <- dat$Correct
data_jags$n <- dat$Trials
data_jags$ID <- dat$ID
data_jags$n_kids <- max(dat$ID) # Añadimos el número de niños a la lista

# Parámetros a monitorear
params <- c("a", "b", "mu", "tau")

# Inicializar, compilar y ejecutar el modelo
# NOTA: Este modelo es complejo y puede tardar uno o dos minutos en ejecutarse
mod <- jags.model(textConnection(mod_string_hierarchical), data = data_jags, n.chains = 3)
update(mod, 1000) # Burn-in
mod_sim <- coda.samples(model = mod, variable.names = params, n.iter = 5000)

# --- 4. Generar y Observar los Diagnósticos de Convergencia ---
# Este comando generará los gráficos que necesitas para responder la pregunta
plot(mod_sim)