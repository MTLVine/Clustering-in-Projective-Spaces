kMeans4.2 = function(x,k){
  n = nrow(x)
  p = ncol(x)
  xU = matrix(data = 0, nrow = n, ncol = p)
  for(i in 1:n){xU[i,] = x[i,]/(sqrt(sum(x[i,]^2)))}
  centres = matrix(data = 1, nrow = k, ncol = p)
  clusters = sample(1:k, n, replace = T)
  for(h in 1:k){
    XtX = t(xU[clusters == h,]) %*% xU[clusters == h,]
    centres[h,] = eigen(XtX)$vectors[,1]
  }
  distances = numeric(n)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  iteration = 0
  while(TRUE){
    iteration = iteration + 1
    centresNew = centres
    for(h in 1:k){
      for(i in 1:n){
        distances[i] = 1 - sum(xU[i,] * centresNew[h,])^2
      }
      centreDistances[,h] = distances
    }
    for(i in 1:n){clusters[i] = which.min(centreDistances[i,])}
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