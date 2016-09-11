# Stat 440
# Case Study 1
# Exploratody Data Analysis
install.packages("qgraph")
library(qgraph)

regions = 68
n = 114
mean(as.matrix(W[1:68, 1:68, 1:114]))
qgraph(W[1:68,1:68,1])
qgraph(W[1:68,1:68,2])

# try normalizing on all 68*68/2 pairs
# this works if all pairs have effects independent of others, but it is likely that
# connections from the same brain region have some relation to eachother 


# Correlations between certain connections. Communities: certain regions are more connected than others



# Exploration: view everything globally (as a percentage for each patient), 
# then average those percentages

percentW = array(0, dim=c(68, 68, 144))
for (i in 1:n){
  s = sum(W[,,i])/2
  for (j in 1:regions){
    for (k in 1:regions){
      percentW[j,k,i] = W[j,k,i]/s;
    }
  }
}

meanW = array(0, dim=c(68, 68))
  for (j in 1:regions){
    for (k in 1:regions){
      meanW[j,k] = mean(W[j,k,]);
    }
  }
qgraph(meanW)

hist(percentW[28,3,], breaks=20)
hist(percentW[28,4,], breaks=20)
hist(percentW[38,23,], breaks=20)

hist(percentW[28,40,], breaks=20)



x <- matrix(0,ncol=68*67/2, nrow=n) # 68 choose 2
colnames <- rep("", 68*67/2)

cur <- 1
#colnamesCur <- 1
for (i in 1:68){
  for (j in 1:68-i){
    for (p in 1:n){
      if (i == i+j){
        print("gonext")
        next;
      }
      print("--")
      print(i)
      print(j)
      print(i+j)
      print(p)
      print(cur)
      x[p,cur] <- percentW[i,i+j,p]
      if (p == n){
        colnames[cur] <- paste(i, j, sep="_");
        cur = cur + 1;
      }
     # cur = cur + 1;
    }
  }
}
x.df <- data.frame(x)

colnames(x) <- colnames
rownames(x) <- c(1:n)

library(reshape2)
x.df$rownum <- 1:n
x.melted <- melt(x.df, id.vars = 'rownum', variable.name = 'region')
ggplot() + geom_tile(data = x.melted, aes(x = rownum, y = region, fill = value))

tmp.df <- x.df
i <- 1

count <- 0
numZero <- 0
for (i in 1:dim(x.df)[2]){
  if(sum(x[,i]) == 0){
    numZero <- numZero + 1
  }
  else{
    count <- count + 1
    tmp.df[,count] <- x.df[,i]
  }
}
newX.df <- tmp.df[,1:count]

/*while (i <= dim(x.df)[2]){
  print("--")
  print(i)
  print(dim(x.df)[2])
  if(sum(tmp.df[,i]) == 0){
    tmp.df[,i] <- NULL
    i = i - 1
  }
}*/

write.csv(x, file = "caseStudy1.csv")



iplotCorr(newX.df, reorder=TRUE)


corrMatrix <- cor(x)
library(reshape2)
corrMatrix$rownum <- colnames(corrMatrix)
corrMatrix.melted <- melt(corrMatrix, id.vars = 'rownum', variable.name = 'region')
ggplot() + geom_tile(data = corrMatrix.melted, aes(x = rownum, y = region, fill = value))
write.csv(corrMatrix, file = "corrMatrix.csv")




install.packages("qtlcharts")
library(qtlcharts)
data(iris)
iris$Species <- NULL
iplotCorr(percentW, reorder=TRUE)
