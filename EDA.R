library(GGally)
library(network)
library(sna)
library(ggplot2)
library(reshape2)

load('Brain_network.RData')

#Calculate number of connections between each pair of regions for an average patient
connections <- lapply(1:114, function(i) {
  return(W[,,i])
})
average.patient <- Reduce('+', connections)/length(connections)
average.patient.df <- data.frame(average.patient)
colnames(average.patient.df) <- 1:68
average.patient.df$rownum <- 1:68

#Create heatmap of average patient brain connections
average.patient.melted <- melt(average.patient.df, id.vars = 'rownum', variable.name = 'region')
average.patient.melted$rownum = factor(average.patient.melted$rownum)
ggplot() + geom_tile(data = average.patient.melted, aes(x = rownum, y = region, fill = value)) + geom_hline(aes(yintercept = 34.5), color = "red") + geom_vline(aes(xintercept = 34.5), color = "red") + geom_line(aes(x = c(0.5, 68.5), y = c(0.5, 68.5)), color = "black") + geom_line(aes(x = c(0.5, 34.5), y = c(34.5, 68.5)), color = "green") + labs(title = "Average patient connection counts", x = 'region', y = "region", fill = "# connections")
#geom_polygon(aes(x = c(0.5, 68.5, 68.5), y = c(0.5, 68.5, 0.5)))

#Create spatial map of region connectivities
load('Coord_Brain.Rdata')
connection.count <- sapply(1:68, function(i) {
  return(sum(average.patient[,i]))
})
spatial.data <- data.frame(x = Coord_Brain[,1], y = Coord_Brain[,2], count = connection.count)
ggplot() + geom_point(data = spatial.data, aes(x = x, y = y, size = count)) + labs(title = "Spatial map of region connectivities", x = "", y = "", size = "#connections")
