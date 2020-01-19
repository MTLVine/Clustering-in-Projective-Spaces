kMeans1 = function(x){
  xRange = range(x)
  centres = runif(2, min = xRange[1], max = xRange[2])
  centre1 = centres[1]
  centre2 = centres[2]
  cluster1 = x[which(abs(x - centre1) < abs(x - centre2))]
  cluster2 = x[which(abs(x - centre1) >= abs(x - centre2))]
  centre1New = centre2New = 0
  while(centre1 != centre1New || centre2 != centre2New){
    centre1 = centre1New
    centre2 = centre2New
    centre1New = mean(cluster1)
    centre2New = mean(cluster2)
    cluster1 = x[which(abs(x - centre1New) < abs(x - centre2New))]
    cluster2 = x[which(abs(x - centre1New) >= abs(x - centre2New))]
  }
  cat("\n")
  print("Cluster 1", quote = F)
  print(cluster1)
  cat("\n")
  print("Cluster 2", quote = F)
  print(cluster2)}

