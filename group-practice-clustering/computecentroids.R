computecentroids <- function(measurescores, idx, K) {
  
  numfeatures <- ncol(measurescores);
  centroids <- data.frame(matrix(nrow=K, ncol=numfeatures));
  
  for(i in 1:K) {
    xind <- which(idx == i);
    centroids[i,] = colSums(measurescores[xind,])/length(xind);
  }
  centroids;
}