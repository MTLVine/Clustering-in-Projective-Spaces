kTest = function(N,q){
  x = g0(1000,2)
  kVector = 1:N
  results = matrix(data = 0, nrow = N, ncol = 5)
  for(i in 1:5){
    for(I in 1:N){results[I,i] = kMeans4.0(x, I)[q]}
  }
  results = cbind(kVector, rowMeans(results))
  if(q == 1){axisLabel = "Number of Iterations"}else{axisLabel = "RMS Distance"}
  plot(results,
       xlab = "Value of k",
       ylab = axisLabel,
       pch = 21,
       bg = "grey")
  lines(results, type = "l", col = 'grey')
  model = lm(results[,2] ~ poly(results[,1], 3, raw = TRUE))
  lines(results[,1], predict(model, data.frame(results[,1])), lty = 3)
}

nTest = function(N,q){
  nVector = numeric(N)
  for(I in 1:N){nVector[I] = I * 10}
  results = matrix(data = 0, nrow = N, ncol = 5)
  for(l in 1:5){
    for(I in 1:N){results[I,l] = kMeans4.0(g0(I * 10, 2), 2)[q]}
  }
  results <- cbind(nVector, rowMeans(results))
  if(q == 1){axisLabel = "Number of Iterations"}else{axisLabel = "RMS Distance"}
  plot(results,
       xlab = "Value of n",
       ylab = axisLabel,
       pch = NA_integer_,
       bg = "grey")
  lines(results, type = "l", col = 'grey')
  model = lm(results[,2] ~ poly(results[,1], 3, raw = TRUE))
  lines(results[,1], predict(model, data.frame(results[,1])), lty = 3)
}



