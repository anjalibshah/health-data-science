featurenormalize <- function(X) {
  
  mu <- colMeans(X);
  X_norm <- apply(X,1,'-',mu);
  
  sigma <- apply(X,2,sd);
  X_norm <- t(apply(X_norm,2,'/',sigma));
  
}