#Introduction image/g0
data = matrix(data = runif(40, min = -3, max = 3), nrow = 20, ncol = 2)
plot(data, xlim = c(-3,3), ylim = c(-3,3), asp = 1, 
     xlab = "Horizontal Component", ylab = "Vertical Component", pch = 21, bg = "grey")
curve(sqrt(1 - x^2), from = -1, to = 1, add = T, lwd = 2)
curve(-sqrt(1 - x^2), from = -1, to = 1, add = T, col = "gray68")
dataU = matrix(data = 0, nrow = 20, ncol = 2)
oldDataU = dataU
for(i in 1:20){
  dataU[i,] = data[i,] / sqrt(sum(data[i,]^2))
  oldDataU[i,] = dataU[i,]
  if(dataU[i,2] < 0){dataU[i,] = -dataU[i,]}
}
origin = matrix(data = 0, nrow = 1, ncol = 2)
points(origin, pch = 21, col = "gray68")
points(dataU, pch = 19)
for(i in which(data[,2] > 0)){
  segments(data[i,1], data[i,2], dataU[i,1], dataU[i,2])
}
for(i in which(data[,2] < 0)){
  segments(data[i,1], data[i,2], oldDataU[i,1], oldDataU[i,2])
  segments(dataU[i,1], dataU[i,2], oldDataU[i,1], oldDataU[i,2], lty = 2, col = "gray68")
}

h1 = rnorm(20, mean = sample(-50:50,1), sd = 10)
v1 = rnorm(20, mean = sample(-50:50,1), sd = 10)
h2 = rnorm(20, mean = sample(-50:50,1), sd = 10)
v2 = rnorm(20, mean = sample(-50:50,1), sd = 10)
data = rbind(cbind(h1,v1),cbind(h2,v2))
plot(data, xlim = c(-3,3), ylim = c(-3,3), asp = 1, 
     xlab = "Horizontal Component", ylab = "Vertical Component", pch = 21, bg = "grey")
curve(sqrt(1 - x^2), from = -1, to = 1, add = T, lwd = 2)
curve(-sqrt(1 - x^2), from = -1, to = 1, add = T, col = "gray68")
dataU = matrix(data = 0, nrow = 40, ncol = 2)
oldDataU = dataU
for(i in 1:40){
  dataU[i,] = data[i,] / sqrt(sum(data[i,]^2))
  oldDataU[i,] = dataU[i,]
  if(dataU[i,2] < 0){dataU[i,] = -dataU[i,]}
}
origin = matrix(data = 0, nrow = 1, ncol = 2)
points(origin, pch = 21, col = "gray68")
points(dataU, pch = 19)
for(i in which(data[,2] > 0)){
  segments(data[i,1], data[i,2], dataU[i,1], dataU[i,2])
}
for(i in which(data[,2] < 0)){
  segments(data[i,1], data[i,2], oldDataU[i,1], oldDataU[i,2])
  segments(dataU[i,1], dataU[i,2], oldDataU[i,1], oldDataU[i,2], lty = 2, col = "gray68")
}


