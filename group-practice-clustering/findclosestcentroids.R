findclosestcentroids <- function(measurescores, initial_centroids) {
  
  K <- nrow(initial_centroids);
  numrows <- nrow(measurescores);
  idx <- data.frame(matrix(nrow=numrows, ncol=1));
  
  for(i in 1:numrows) {
    
    dist <- data.frame(matrix(nrow=K,ncol=1));
    
    for (j in 1:K) {
      
      dist[j,1] <- norm(as.matrix(measurescores[i,] - initial_centroids[j,]), "F");
    }
    
    idx[i,1] <- which.min(dist[,1]);
  }
  
  idx;
}