runKMeanswithVisualization <- function(measurescores, max_iters=10) {
  
  K <- 3;
  numrows <- nrow(measurescores);
  idx <- data.frame(matrix(nrow=numrows, ncol=1));
  par(mfrow = c(2, 5));
  par(cex = 0.6);
  par(mar = c(0, 0, 0, 0), oma = c(4, 4, 0.5, 0.5));
  par(mgp = c(2, 0.6, 0));
  
  ms_norm <- featurenormalize(measurescores);
  colnames(ms_norm) <- NULL;
  
  optimalcentroids <- determineoptimalcentroids(ms_norm);
  print("Optimal centroids = ");
  print(optimalcentroids);
  cat("\n");
  
  library(labdsv);
  
  Zscores <- pca(ms_norm, dim=2);
  
  centroidindex1 <- optimalcentroids[1];
  centroidindex2 <- optimalcentroids[2];
  centroidindex3 <- optimalcentroids[3];
  
  init_c1 <- cbind(Zscores$scores[centroidindex1,1],Zscores$scores[centroidindex1,2]);
  init_c2 <- cbind(Zscores$scores[centroidindex2,1],Zscores$scores[centroidindex2,2]);
  init_c3 <- cbind(Zscores$scores[centroidindex3,1],Zscores$scores[centroidindex3,2]);
  
  centroids <- rbind(init_c1, init_c2, init_c3);
  
  par(oma = c(1.5,1.5,3,.25));
  
  for(i in 1:max_iters){
    
    print(paste("K-Means iteration", i, "/", max_iters, "..."));
    cat("\n");
    
    centroids <- centroids[order(-centroids[,1],-centroids[,2]),];
    
    idx = findclosestcentroids(Zscores$scores, centroids);
    
    
    plotprogresskmeans(Zscores$scores, centroids,idx,K, i);
    
    centroids = computecentroids(Zscores$scores, idx, K);
    print("Centroids = ");
    print(centroids);
    cat("\n");
    
  }

  clusterinfo <- list(idx=idx, centroids=centroids);
}
