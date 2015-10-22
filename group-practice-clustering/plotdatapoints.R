plotdatapoints <- function(X, idx, K=3) {
  
  X <- cbind(X, idx);
  col.list <- c("green", "blue", "red");
  palette(col.list);
  plot(X[,2], X[,1], axes=FALSE, frame.plot=TRUE, pch=19, col= X[,3]);
}