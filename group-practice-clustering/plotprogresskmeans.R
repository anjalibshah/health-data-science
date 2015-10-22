plotprogresskmeans <- function(X, centroids, previous_centroids, idx, K, i) {
  
  plotdatapoints(X, idx);
  points(centroids[,2], centroids[,1], col="black", pch=4, lwd=2);
  if(i %in% c(1,6))
    axis(2, at = seq(0, 100, 20));
  if(i %in% c(6,7,8,9,10))
    axis(1, at = seq(0, 100, 20));
}