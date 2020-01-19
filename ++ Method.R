kMeans4.3 = function(x,k){
  n = nrow(x)
  p = ncol(x)
  xU = matrix(data = 0, nrow = n, ncol = p)
  for(i in 1:n){xU[i,] = x[i,]/(sqrt(sum(x[i,]^2)))}
  centres = matrix(data = 0, nrow = k, ncol = p)
  centreDistances = matrix(data = 0, nrow = n, ncol = k)
  clusters = distances = numeric(n)
  for(h in 1:k){
    iteration = h
    if(iteration == 1){P = NULL}else{P = abs(closestDistances)}
    centres[h,] = xU[sample(1:n, 1, prob = P),]
    closestDistances = numeric(n)
    for(i in 1:n){
      centreDistances[i,h] = 1 - sum(xU[i,] * centres[h,])^2
    }
    knownDistances = data.frame(centreDistances[,1:h])
    for(i in 1:n){closestDistances[i] = min(knownDistances[i,])}
  }
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
