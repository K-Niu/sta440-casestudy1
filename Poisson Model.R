library(R2jags)
library(R2WinBUGS)
library(ggplot2)

load('poisson_data.Rdata')
load('Brain_network.RData')

data.list <- list(I = dataset[,"i"], 
                  U = dataset[,"u"], 
                  V = dataset[,"v"], 
                  Y = dataset[,"y"], 
                  n = dim(dataset)[1],
                  num.patients = 114,
                  num.regions = 68,
                  I2 = covariate[,"INDICATOR"],
                  IQ = covariate[,"FSIQ"])

model <- "
model {
##
#Likelihood
##
for(k in 1:n) {
nu[k] <- alpha[I[k]] + lambda*abs(z[U[k]] - z[V[k]])
Y[k] ~ dpois(nu[k])
}
sd.q ~ dt(0, 0.1, 1) T(0,)
phi.q <- pow(sd.a, -2)
for(i in 1:num.patients) {
IQ[i] ~ dnorm(beta[1] + beta[2]*alpha[I2[i]], phi.q)
}

##
#Priors for z's
##
for(u in 1:num.regions) {
z[u] ~ dnorm(0, 1)
}

##
#Priors for alpha's
##
sd.a ~ dt(0, 0.1, 1) T(0,)
phi.a <- pow(sd.a, -2)
for(i in 1:num.patients) {
alpha[i] ~ dnorm(0, phi.a)
}

##
#Prior for lambda
##
sd.l ~ dt(0, 0.1, 1) T(0,)
phi.l <- pow(sd.l, -2)
lambda ~ dnorm(0, phi.l) T(,0)

##
#Priors for beta's
##
sd.b ~ dt(0, 0.1, 1) T(0,)
phi.b <- pow(sd.l, -2)
beta[1] ~ dnorm(0, phi.b)
beta[2] ~ dnorm(0, phi.b)
}
"

inits <- function() {
  z <- rep(0, data.list$num.regions)
  alpha <- rep(0.1, data.list$num.patients)
  lambda <- 0
  beta <- rep(0, 2)
  sd.a <- 0.1
  sd.l <- 0.1
  sd.q <- 0.1
  sd.b <- 0.1
  return(list(z = z, alpha = alpha, lambda = lambda, beta = beta, sd.a = sd.a, sd.l = sd.l, sd.q = sd.q, sd.b = sd.b))
}

parameters <- c("z", "alpha", "lambda", "beta", "sd.a", "sd.l", "sd.q", "sd.b")

game.sim <- jags(data.list, inits = inits, parameters, model.file = textConnection(model), n.iter = 5000)
game.bugs <- as.mcmc(game.sim$BUGSoutput$sims.matrix)

#Visualize posterior distributions
load('model_output_poisson.Rdata')

#Posterior distributions (beta[1] and beta[2] seem to be the only non-weird ones...)
ggplot() + geom_density(aes(x = as.vector(game.bugs[,"z[1]"]))) + labs(title = "z[1] posterior distribution", x = "z[1]")

#Trace plots
ggplot() + geom_line(aes(x = 1:3750, y = as.vector(game.bugs[,"z[1]"]))) + labs(title = "z[1] trace plot", x = "iteration", y = "value")
