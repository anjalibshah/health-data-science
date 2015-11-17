determineoptimalcentroids <- function(ms_norm, K=3, max_iters=10){
  
  numpractices <- nrow(ms_norm);
  numfeatures <- ncol(ms_norm);
  uniq_ms_norm <- cbind(ms_norm, 1:numpractices);
  uniq_ms_norm <- uniq_ms_norm[!duplicated(uniq_ms_norm[,1:4]),];
  rownames(uniq_ms_norm) <- uniq_ms_norm[,5];
  uniquerows <- as.numeric(rownames(uniq_ms_norm));
  
  clusterinfo <- list();
  dist <- list();
  
  for(i in 1:2) {
  
    print(paste("Random initialization iteration of centroids - ", i, "/", 2, "..."));
    cat("\n");
    
    randidx <- sample(uniquerows, K);
    
    centroidindex1 <- randidx[1];
    centroidindex2 <- randidx[2];
    centroidindex3 <- randidx[3];
    
    init_c1 <- cbind(ms_norm[centroidindex1,1],ms_norm[centroidindex1,2],
                     ms_norm[centroidindex1,3],ms_norm[centroidindex1,4]);
    init_c2 <- cbind(ms_norm[centroidindex2,1],ms_norm[centroidindex2,2],
                     ms_norm[centroidindex2,3],ms_norm[centroidindex2,4]);
    init_c3 <- cbind(ms_norm[centroidindex3,1],ms_norm[centroidindex3,2],
                     ms_norm[centroidindex3,3],ms_norm[centroidindex3,4]);
    centroids <- rbind(init_c1, init_c2, init_c3);
    
    for(j in 1:max_iters){
      idx = findclosestcentroids(ms_norm, centroids);
      centroids = computecentroids(ms_norm, idx, K);
    }
    
    print("Randomly initialized Centroids = ");
    print(centroids);
    cat("\n");
    
    for (k in 1:numpractices) {
      cind <- idx[k,1];
      dist <- norm(as.matrix(ms_norm[k,] - centroids[cind,]), "F");
    }
  
    costfunction <- sum(dist^2)/numpractices;
    print("Cost function = ");
    print(costfunction);
    cat("\n");
  
    optimalcentroids <- cbind(centroidindex1, centroidindex2, centroidindex3);
  
    clusterinfo$costfunction <- costfunction;
    clusterinfo$optcentroids <- optimalcentroids;
  
  }
  
  optidx <- which.min(clusterinfo$costfunction);
  optimalcentroids <- clusterinfo$optcentroids[optidx,];
  
}