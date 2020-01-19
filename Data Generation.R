g0 = function(n,p){
  matrix(data = runif(n * p, min = -50, max = 50), nrow = n, ncol = p)
}

g1 = function(n,p,k){
  x = matrix(data = 0, nrow = n, ncol = p)
  for(h in 1:k){
    xSubset = matrix(data = 0, nrow = n/k, ncol = p)
    for(j in 1:p){xSubset[,j] = rnorm(n/k, mean = sample(-50:50, 1), sd = 10)}
    x[(((n/k)*(h-1)) + 1) : ((n/k)*h),] = xSubset
  }
  x
}














