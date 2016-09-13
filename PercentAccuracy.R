load("/Users/calliemao/Downloads/probit_data.Rdata")
load("/Users/calliemao/Downloads/model_output_one_lambda.Rdata")

install.packages("ggplot2")
library(ggplot2)
ggplot() + geom_density(aes(x = as.vector(game.bugs[,"alpha[1]"])))

meanAlpha <- vector()
for (i in 1:114){
  alphaStr <- capture.output(cat("alpha[",i,"]",sep=""))
  meanAlpha[i] <- mean(game.bugs[, alphaStr]) #cat("\"alpha[",i,"]\"",sep="")])
}

meanZ <- vector()
for (i in 1:68){
  zStr <- capture.output(cat("z[",i,"]",sep=""))
  meanZ[i] <- mean(game.bugs[, zStr])
}

meanLambda <- mean(game.bugs[, "lambda"])
n <- 114
predictedProb <- matrix(0,ncol=4, nrow=n*68*67/2) # 68 choose 2
colnames(predictedProb) <- c("patient", "u", "v", "probConnected")

cur <- 1
#colnamesCur <- 1
for (i in 1:68){
  for (j in i+1:68){
    for (p in 1:n){
   #   print("---")
  #    print(i)
  #    print(j)
  #    print(p)
      if (j > 68)
        next;
      predictedProb[cur, "patient"] <- p
      predictedProb[cur, "u"] <- i
      predictedProb[cur, "v"] <- j
      predictedProb[cur, "probConnected"] <- pnorm(meanAlpha[p] + meanLambda * abs(meanZ[i] - meanZ[j]))
    #  print("-")
    #  print(meanZ[i])
    #  print(meanZ[j])
    #  print(abs(meanZ[i] - meanZ[j]))
    #  print(meanLambda)
    #  print(meanAlpha[p])
    #  print(meanAlpha[p] + meanLambda * abs(meanZ[i] - meanZ[j]))
    #  print(pnorm(meanAlpha[p] + meanLambda * abs(meanZ[i] - meanZ[j])))
      cur = cur + 1
    }
  }
}
predictedProb.df <- data.frame(predictedProb)
View(predictedProb.df)

sum(predictedProb.df["probConnected"] > 0.5)