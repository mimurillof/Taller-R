# --- 1. Cargar Librerías y Preparar Datos ---
library("MASS")
library("rjags")

# Cargar el conjunto de datos OME
data("OME")

# Limpiar datos y reformatear variables
dat <- subset(OME, OME != "N/A")
dat$OME <- factor(dat$OME) # re-etiquetar OME
dat$ID <- as.numeric(factor(dat$ID)) # re-etiquetar ID de 1 a 63

# Crear la matriz de covariables (X)
mod_glm <- glm(Correct / Trials ~ Age + OME + Loud + Noise, data = dat, weights = Trials, family = "binomial")
X <- model.matrix(mod_glm)[, -1]


# --- 2. Especificación del Modelo Jerárquico en JAGS ---
mod_string_hierarchical <- "
model {
    # Nivel 1: Likelihood
    for (i in 1:length(y)) {
        y[i] ~ dbin(phi[i], n[i])
        logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
    }
    
    # Nivel 2: Priors para los interceptos de cada niño
    for (j in 1:n_kids) {
        a[j] ~ dnorm(mu, prec_a)
    }
    
    # Nivel 3: Hiperpriores
    mu ~ dnorm(0.0, 1.0/10.0^2)
    prec_a ~ dgamma(0.5, 0.5)
    tau <- sqrt(1.0 / prec_a)
    
    # Priors para los coeficientes fijos (betas)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1.0/4.0^2)
    }
}
"

# --- 3. Preparación y Ejecución del Modelo ---
data_jags <- as.list(as.data.frame(X))
data_jags$y <- dat$Correct
data_jags$n <- dat$Trials
data_jags$ID <- dat$ID
data_jags$n_kids <- max(dat$ID)

params <- c("a", "b", "mu", "tau")

# NOTA: Asegúrate de que las cadenas hayan convergido razonablemente antes de calcular el DIC.
# Aumentar 'update' y 'n.iter' si la autocorrelación era muy alta.
mod <- jags.model(textConnection(mod_string_hierarchical), data = data_jags, n.chains = 3)
update(mod, 5000) # Aumentamos el burn-in por la autocorrelación
mod_sim <- coda.samples(model = mod, variable.names = params, n.iter = 10000) # Aumentamos las iteraciones


# --- 4. Calcular el DIC para el Modelo Jerárquico ---
cat("\n--- Calculando DIC para el modelo jerárquico ---\n")
dic_hierarchical <- dic.samples(mod, n.iter = 5000)
print(dic_hierarchical)