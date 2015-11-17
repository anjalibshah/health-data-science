plotprogresskmeans <- function(X, centroids, idx, K, i) {
  
  plotdatapoints(X, idx);
  points(centroids[,2], centroids[,1], col="black", pch=4, lwd=2);
  if(i %in% c(3))
    legend(x=-3,y=5.7, legend=c("High", "Mid", "Low", "centroid"), col=c("green", "blue", "red", "black"), 
         xpd=NA, horiz=TRUE, bty="n",
         pch=c(19,19,19,4), pt.lwd=2, title="Group Practice Rankings");
  if(i %in% c(1,6))
    #axis(2, at = seq(0, 100, 20));
    axis(2, at = seq(-2, 5, 2));
  if(i %in% c(6,7,8,9,10))
    #axis(1, at = seq(0, 100, 20));
    axis(1, at = seq(-3, 2, 1));
}