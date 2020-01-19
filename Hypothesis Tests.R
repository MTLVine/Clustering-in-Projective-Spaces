zTest = function(a,b,k,q){
  x = g1(100,2,k)
  aResults = bResults = numeric(100)
  for(i in 1:100){
    aResults[i] = switch(a, kMeans4.1(x,k)[q], kMeans4.2(x,k)[q], kMeans4.3(x,k)[q])
    bResults[i] = switch(b, kMeans4.1(x,k)[q], kMeans4.2(x,k)[q], kMeans4.3(x,k)[q])
  }
  z = (mean(aResults) - mean(bResults)) / sqrt((var(aResults)/100) + (var(bResults)/100))
  cat("Mean a:", mean(aResults), sep = " ")
  cat("\n")
  cat("Mean b:", mean(bResults), sep = " ")
  cat("\n")
  if(var(aResults) & var(bResults) == 0){cat("Cannot reject null hypothesis")}
  else{if(z > qnorm(0.95)){
    cat("Null hypothesis rejected")
  }else{cat("Cannot reject null hypothesis")
  }}
}
