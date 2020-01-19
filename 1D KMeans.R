closestCentre = function(x,y){
  y[which(abs(x - y) == min(abs(x - y)))]}

kMeans2 = function(x,k){
  K = k
  n = length(x)
  xRange = range(x)
  centres = runif(k, min = xRange[1], max = xRange[2])
  centresNew = 1:k
  xCentres = 1:n
  while(TRUE){
     centresNew = centres
     for(i in 1:n){xCentres[i] = closestCentre(x[i],centres)}
     for(j in 1:k){
       centres[j] = mean(x[xCentres == centresNew[j]])}
     if(any(is.nan(centres))){
       centres = centres[-which(is.nan(centres))]
       k = length(centres)
       centresNew = numeric(k)}
     if(all(centres == centresNew))break}
  for(j in 1:k){
    cat("\n")
    print(paste("Cluster", j, sep =" "), quote = F)
    print(x[xCentres == centresNew[j]])
  }
  if(k != K){
      cat("\n")
      cat("1 or more clusters were empty")
  }
  plot(x,xCentres, pch = 20,
      xlab = "Observation",
      ylab = "Cluster Centre for Observation",
      main = "Clustering of Data Set 'x'")
  }



 




  
