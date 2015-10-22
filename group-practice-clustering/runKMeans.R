runKMeans <- function(measurescores, initial_centroids, max_iters=10) {
  
  K <- nrow(initial_centroids);
  numrows <- nrow(measurescores);
  idx <- data.frame(matrix(nrow=numrows, ncol=1));
  centroids = initial_centroids;
  previous_centroids = centroids;
  par(mfrow = c(2, 5));
  par(cex = 0.6);
  par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5));
  par(mgp = c(2, 0.6, 0));
  #par(xaxt='n', yaxt='n');
  
  for(i in 1:max_iters){
    
    print(paste("K-Means iteration", i, "/", max_iters, "..."));
    cat("\n");
    
    idx = findclosestcentroids(measurescores, centroids);
    
    plotprogresskmeans(measurescores[, c(1,3)], centroids[,c(1,3)],
                       previous_centroids[,c(1,3)],idx,K, i);
    
    previous_centroids = centroids;
    centroids = computecentroids(measurescores, idx, K);
    print("Centroids = ");
    print(centroids);
    cat("\n");
    
  }
  
  clusterinfo <- list(idx=idx, centroids=centroids);
}