kMeans1 = function(x){
  n = length(x)
  xRange = range(x)
  centres = runif(2, min = xRange[1], max = xRange[2])
  clusters = distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = 2)
  while(TRUE){
    centresNew = centres
    for(h in 1:2){
      for(i in 1:n){
        distances[i] = abs(x[i] - centresNew[h])
      }
      centreDistances[,h] = distances
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,])}
    for(h in 1:2){centres[h] = mean(x[clusters == h,])}
    if(any(is.nan(centres))){
      centres = centres[-which(is.nan(centres))]
      k = 1
      centresNew = numeric(1)
    }
    if(all(centres == centresNew))break
  }
  data.frame(x, clusters)
}

kMeans2 = function(x,k){
  kInitial = k
  n = length(x)
  xRange = range(x)
  centres = runif(k, min = xRange[1], max = xRange[2])
  clusters = distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  while (TRUE){
    centresNew = centres
    for(h in 1:k){
      for(i in 1:n){
        distances[i] = abs(x[i] - centresNew[h])
      }
      centreDistances[,h] = distances
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,])}
    for(h in 1:k){centres[h] = mean(x[clusters == h])}
    if(any(is.nan(centres))){
      centres = centres[-which(is.nan(centres))]
      k = length(centres)
      centresNew = numeric(k)
    }
    if (all(centres == centresNew)) break
  }
  data.frame(x, clusters)
  if(k != kInitial){
    cat("\n")
    cat("1 or more clusters were empty")}
}

kMeans3 = function(x,k){
  kInitital = k
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
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,])}
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
  data.frame(x, clusters)
  if(k != kInitial){
    cat("\n")
    cat("1 or more clusters were empty")}
}
