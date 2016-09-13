library(R2jags)
library(R2WinBUGS)
library(ggplot2)

load('probit_data.Rdata')

data.list <- list(I = dataset[,"i"], 
                  U = dataset[,"u"], 
                  V = dataset[,"v"], 
                  Y = dataset[,"y"], 
                  n = dim(dataset)[1],
                  num.patients = 114,
                  num.regions = 68)

model <- "
model {
##
#Likelihood
##
for(k in 1:n) {
p[k] <- pnorm(alpha[I[k]] + lambda*abs(z[U[k]] - z[V[k]]), 0, 1)
Y[k] ~ dbinom(p[k], 1)
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

}
"

inits <- function() {
  z <- rep(0, data.list$num.regions)
  alpha <- rep(0, data.list$num.patients)
  lambda <- 0
  sd.a <- 0.1
  sd.l <- 0.1
  return(list(z = z, alpha = alpha, lambda = lambda, sd.a = sd.a, sd.l = sd.l))
}

parameters <- c("z", "alpha", "lambda", "sd.a", "sd.l")

game.sim <- jags(data.list, inits = inits, parameters, model.file = textConnection(model), n.iter = 5000)
game.bugs <- as.mcmc(game.sim$BUGSoutput$sims.matrix)

#View posterior parameter estimates
load('model_output_one_lambda.Rdata')
#For example alpha[1]
#Can choose from alpha[1], ..., alpha[114], lamda, z[1], ..., z[68]
ggplot() + geom_density(aes(x = as.vector(game.bugs[,"alpha[1]"])))

