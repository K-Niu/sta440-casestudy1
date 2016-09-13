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
ggplot() + geom_density(aes(x = as.vector(game.bugs[,"z[1]"]))) + labs(title = "z[1] posterior distribution", x = "z[1]")

#Trace plots
ggplot() + geom_line(aes(x = 1:3750, y = as.vector(game.bugs[,"z[1]"]))) + labs(title = "z[1] trace plot", x = "iteration", y = "value")

#Get mean locations of each region in one-dimensional latent space
locations <- sapply(1:68, function(i) {
  mean(game.bugs[,paste0("z[", i,"]")])
})

#Plot locations of each region in latent space
ggplot() + geom_point(aes(x = 1:68, y = locations)) + labs(title = "Region location in latent space", x = "region", y = "location")

#Compare latent space location to physical space location
load('Coord_Brain.Rdata')
distance.comparison <- data.frame(pairs = character(), physical.distance = numeric(), latent.distance = numeric)
for(i in 1:67) {
  for(j in (i+1):68) {
    p.dist = sqrt((Coord_Brain[i,1] - Coord_Brain[j,1])^2 + (Coord_Brain[i,2] - Coord_Brain[j,2])^2)
    l.dist = abs(locations[i] - locations[j])
    distance.comparison = rbind(distance.comparison, data.frame(pairs = paste0(i, ":", j), physical.distance = p.dist, latent.distance = l.dist))
  }
}
ggplot() + geom_point(data = distance.comparison, aes(x = latent.distance, y = physical.distance)) + labs(title = "Pairwise brain region distances in latent space vs. physical space", x = "latent distance", y = "physical distance")
