rankgrouppractices <- function() {
  
  #Reading columns 1 through 5 as strings and 6 through 9 as numeruc
  measurescores = read.xlsx(paste(getwd(), "measures.xlsx", sep="//"), 1, 
                            colClasses = c(rep("character",5), rep("numeric",4)));
  
  # Converting columns read as factors into characters
  fctr.cols <- sapply(measurescores, is.factor);
  measurescores[, fctr.cols] <- sapply(measurescores[, fctr.cols], as.character);
  #print(lapply(measurescores, class));
  
  K <- 3;
  init_c1 <- cbind(81, 89, 94, 97);
  init_c2 <- cbind(69, 63, 93, 91);
  init_c3 <- cbind(52, 48, 88, 72);
  initial_centroids <- rbind(init_c1, init_c2, init_c3);
  
  #idx <- findclosestcentroids(measurescores[,6:9], initial_centroids);
  #print(idx);
  #centroids <- computecentroids(measurescores[,6:9], idx, K);
  #print(centroids);
  
  clusterinfo <- runKMeans(measurescores[,6:9], initial_centroids);
  
  #print(clusterinfo$idx);
  #print(clusterinfo$centroids);
  
  rank <- clusterinfo$idx;
  measurescores <- cbind(measurescores, rank);
  colnames(measurescores)[10] <- "Rank";
  
  sortedscores <- measurescores[order(measurescores[,c("Rank")], 
                                      measurescores[, c("State")], 
                                      measurescores[, 1]),];
  
  totalhighrank <- nrow(subset(sortedscores, Rank == 1));
  totalmidrank <- nrow(subset(sortedscores, Rank == 2));
  totallowrank <- nrow(subset(sortedscores, Rank == 3));
  
  totalHReRx <- (nrow(subset(sortedscores, Rank == 1 & Participating.in.eRx == 'Y' ))/totalhighrank)*100;
  cat("Percentage of high ranked practices participating in eRx: ");
  cat(paste(format(round(totalHReRx,2), nsmall=2), "%", "\n"));
  
  totalMReRx <- (nrow(subset(sortedscores, Rank == 2 & Participating.in.eRx == 'Y' ))/totalmidrank)*100;
  cat("Percentage of mid ranked practices participating in eRx: ");
  cat(paste(format(round(totalMReRx,2), nsmall=2), "%", "\n"));
  
  totalLReRx <- (nrow(subset(sortedscores, Rank == 3 & Participating.in.eRx == 'Y' ))/totallowrank)*100;
  cat("Percentage of low ranked practices participating in eRx: ");
  cat(paste(format(round(totalLReRx,2), nsmall=2), "%", "\n"));
  
  cat("\n");
  
  totalHRPQRS <- (nrow(subset(sortedscores, Rank == 1 & Participating.in.PQRS == 'Y' ))/totalhighrank)*100;
  cat("Percentage of high ranked practices participating in PQRS: ");
  cat(paste(format(round(totalHRPQRS,2), nsmall=2), "%", "\n"));
  
  totalMRPQRS <- (nrow(subset(sortedscores, Rank == 2 & Participating.in.PQRS == 'Y' ))/totalmidrank)*100;
  cat("Percentage of mid ranked practices participating in PQRS: ");
  cat(paste(format(round(totalMRPQRS,2), nsmall=2), "%", "\n"));
  
  totalLRPQRS <- (nrow(subset(sortedscores, Rank == 3 & Participating.in.PQRS == 'Y' ))/totallowrank)*100;
  cat("Percentage of low ranked practices participating in PQRS: ");
  cat(paste(format(round(totalLRPQRS,2), nsmall=2), "%", "\n"));
}