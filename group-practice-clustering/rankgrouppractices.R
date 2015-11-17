rankgrouppractices <- function() {
  
  # Calls to required packages
  library("xlsx");
  library("xlsxjars");
  
  # Reading columns 1 through 5 as strings and 6 through 9 as numeruc
  measurescores = read.xlsx(paste(getwd(), "measures.xlsx", sep="//"), 1, 
                            colClasses = c(rep("character",5), rep("numeric",4)));
  
  # Converting columns read as factors into characters
  fctr.cols <- sapply(measurescores, is.factor);
  measurescores[, fctr.cols] <- sapply(measurescores[, fctr.cols], as.character);
  #print(lapply(measurescores, class));
  
  # Run K-Means algorithm
  clusterinfo <- runKMeanswithVisualization(measurescores[,6:9]);
  
  # Obtain cluster information and rankings
  rank <- clusterinfo$idx;
  measurescores <- cbind(measurescores, rank);
  colnames(measurescores)[10] <- "Rank";
  
  # Sort measure scores by rank
  sortedscores <- measurescores[order(measurescores[,c("Rank")], 
                                      measurescores[, c("State")], 
                                      measurescores[, 1]),];
  
  #Calculate eRx and PQRS participation statistics for ranked practices
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
  cat("\n");
  
  # Plot rank statistics
  dev.new();
  par(mfrow = c(2, 1));
  barplot(c(totalHReRx,totalMReRx,totalLReRx), 
          col=c("green","blue","red"), legend=c("High", "Mid", "Low"),
          xlab=" eRx Participation Statistics by Rank", ylim=c(0,60),
          args.legend = list(x = "topright", bty = "y", inset=c(0, 0)));
  barplot(c(totalHRPQRS,totalMRPQRS,totalLRPQRS), 
          col=c("green","blue","red"),
          xlab=" PQRS Participation Statistics by Rank", ylim=c(0,120));
  
  # Plot justification for dimensionality reduction
  dev.new();
  svd1 <- svd(scale(measurescores[,6:9]));
  par(mfrow = c(1,2));
  plot(svd1$d,pch=19, xlab="Column", ylab="Singular Value");
  plot(svd1$d^2/sum(svd1$d^2), pch=19, xlab="Column", 
       ylab="Percentage of variance explained");
  
  cat("\n");
  #cat("Subset of high ranked hospitals sorted alphabetically: ");
  #cat("\n");
  #highrankedpractices <- subset(sortedscores, Rank ==1);
  #print(highrankedpractices[1:5,1]);
  #cat("\n");
  
  #cat("Subset of mid ranked hospitals sorted alphabetically: ");
  #cat("\n");
  #midrankedpractices <- subset(sortedscores, Rank ==2);
  #print(midrankedpractices[1:5,1]);
  #cat("\n");
  
  #cat("Subset of low ranked hospitals sorted alphabetically: ");
  #cat("\n");
  #lowrankedpractices <- subset(sortedscores, Rank ==3);
  #print(lowrankedpractices[1:5,1]);
  #cat("\n");
  
  cat("Ranking of hospitals within NY: ");
  cat("\n");
  NYpractices <- subset(sortedscores, State == "NY");
  print(NYpractices[,c(1,3,10)]);
  cat("\n");
  
  #cat("\n");
  #cat("Top ranked practices by state: ");
  #toprankedpractices <- subset(sortedscores, 
                               #State %in% c("CA", "MN", "PA", "OH", "FL", "NY", "MA", "MI", "TX", "MO", "WI", "TN", "IN", "SD", "NC", "MN")
                               #& Rank == 1);
  #print(toprankedpractices[,c(1,3,10)]);
  #cat("\n");
  
}