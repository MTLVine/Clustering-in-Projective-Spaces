kMeans3.0 = function(x,k){
  K = k
  n = nrow(x)
  p = ncol(x)
  xRanges = matrix(data = 0, nrow = 2, ncol = p)
  for(j in 1:p){xRanges[,j] = range(x[,j])}
  centres = matrix(data = 0, nrow = k, ncol = p)
  for(j in 1:p){centres[,j] = runif(k, min = xRanges[1,j], max = xRanges[2,j])}
  clusters = distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  while(TRUE){
    centresNew = centres
    for(h in 1:k){
      for(i in 1:n){
        distances[i] = sqrt(sum((x[i,] - centresNew[h,])^2))
      }
      centreDistances[,h] = distances
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,1:k])}
    for(h in 1:k){
      centres[h,] = colMeans(matrix(data = x[clusters == h,], ncol = p))
    }
    if(any(is.nan(centres))){
      centres = matrix(data = centres[-which(is.nan(centres)),], ncol = p)
      k = nrow(centres)
      centresNew = matrix(data = 0, nrow = nrow(centres), ncol = p)
    }
    if(all(centres == centresNew))break
  }
  for(h in 1:k){
    cat("\n")
    print(paste("Cluster", h, sep = " "), quote = F)
    print(x[clusters == h,])
  }
  if(k != kInitial){
    cat("\n")
    cat("1 or more clusters were empty")}
}

k = 2
x = g1(10,2,2)
plot(x, xlab = "Horizontal Component", ylab = "Vertical Component", pch = 21, bg = "grey")

K = k
n = nrow(x)
p = ncol(x)
xRanges = matrix(data = 0, nrow = 2, ncol = p)
for(j in 1:p){xRanges[,j] = range(x[,j])}
centres = matrix(data = 0, nrow = k, ncol = p)
for(j in 1:p){centres[,j] = runif(k, min = xRanges[1,j], max = xRanges[2,j])}
points(centres[,1], centres[,2], pch = 9)
clusters = distances = numeric(n)
centreDistances = matrix(data = 0, nrow = n, ncol = k)
  
centresNew = centres
for(h in 1:k){
  for(i in 1:n){
    distances[i] = sqrt(sum((x[i,] - centresNew[h,])^2))
    }
  centreDistances[,h] = distances
}
for(i in 1:n){clusters[i] = which.min(centreDistances[i,1:k])}
plot(x[,1], x[,2], xlab = "Horizontal Component", ylab = "Vertical Component", pch = 21)
points(x[clusters == 2,1], x[clusters == 2,2], pch = 19)
points(centres[,1], centres[,2], pch = 9)
for(h in 1:k){
  centres[h,] = colMeans(matrix(data = x[clusters == h,], ncol = p))
}

plot(x[,1], x[,2], xlab = "Horizontal Component", ylab = "Vertical Component", pch = 21)
points(x[clusters == 2,1], x[clusters == 2,2], pch = 19)
points(centres[,1], centres[,2], pch = 9)





