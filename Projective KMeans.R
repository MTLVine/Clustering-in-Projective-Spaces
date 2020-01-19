kMeans4 = function(x,k){
  indicator = 0
  n = nrow(x)
  p = ncol(x)
  xU = matrix(data = 0, nrow = n, ncol = p)
  for(i in 1:n){xU[i,] = x[i,]/(sqrt(sum(x[i,]^2)))}
  xRanges = matrix(data = 0, nrow = 2, ncol = p)
  for(j in 1:p){xRanges[,j] = range(xU[,j])}
  centres = matrix(data = 0, nrow = k, ncol = p)
  for(j in 1:p){centres[,j] = runif(k, min = xRanges[1,j], max = xRanges[2,j])}
  for(h in 1:k){centres[h,] = centres[h,]/sqrt(sum(centres[h,]^2))}
  clusters = distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  while(TRUE){
    centresNew = centres
    for(h in 1:k){
      for(i in 1:n){
        distances[i] = 1 - sum(xU[i,] * centresNew[h,])^2
      }
      centreDistances[,h] = abs(distances)
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,1:k])}
    for(h in 1:k){
      if(!any(clusters == h)){indicator = 1}
      XtX = t(xU[clusters == h,]) %*% xU[clusters == h,]
      centres[h,] = eigen(XtX)$vectors[,1]
    }
    if(all(centres == centresNew))break
  }
  print(data.frame(xU, clusters))
  if(indicator == 1){
    cat("\n")
    cat("1 or more clusters were empty")}
}

kMeans4.0 = function(x,k){
  n = nrow(x)
  p = ncol(x)
  xU = matrix(data = 0, nrow = n, ncol = p)
  for(i in 1:n){xU[i,] = x[i,]/(sqrt(sum(x[i,]^2)))}
  xRanges = matrix(data = 0, nrow = 2, ncol = p)
  for(j in 1:p){xRanges[,j] = range(xU[,j])}
  centres = matrix(data = 0, nrow = k, ncol = p)
  for(j in 1:p){centres[,j] = runif(k, min = xRanges[1,j], max = xRanges[2,j])}
  for(h in 1:k){centres[h,] = centres[h,]/sqrt(sum(centres[h,]^2))}
  clusters = distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  iteration = 0
  while(TRUE){
    iteration = iteration + 1
    centresNew = centres
    for(h in 1:k){
      for(i in 1:n){
        distances[i] = 1 - sum(xU[i,] * centresNew[h,])^2
      }
      centreDistances[,h] = abs(distances)
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,1:k])}
    for(h in 1:k){
      XtX = t(xU[clusters == h,]) %*% xU[clusters == h,]
      centres[h,] = eigen(XtX)$vectors[,1]
    }
    if(all(centres == centresNew))break
  }
  finalDistances = numeric(n)
  for(i in 1:n){
    finalDistances[i] = min(centreDistances[i,])
  }
  c(iteration, sqrt(sum(finalDistances)/n))
}
