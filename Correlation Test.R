f = function(a, k, n){
  m = matrix(data = 0, nrow = n, ncol = 2)
  x = matrix(data = runif(1000, min = 0, max = 100), nrow = 500, ncol = 2)
  for(i in 1:n){
    m[i,] = switch(a, kMeans4.1(x,k), kMeans4.2(x,k), kMeans4.3(x,k))
  }
  plot(m, xlab = "Number of Iterations", ylab = "Total Distance", pch = 21, bg = "grey")
}

