mushroom <- read.csv("agaricus-lepiota.csv",header = TRUE) # load dataset
#show missing data in a table
sums <- NULL
for(i in 1:length(mushroom)){
  sums <- c(sums, sum(is.na(mushroom[,i])))
}
numMissData <- data.frame("column" = columnNames, "numberOfMissingData" = sums )
missmap(mushroom, main = "Missing values vs observed") # plot missing data distrubition
rm(i, sums, numMissData)

#detect outliers
# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
OutVals = boxplot(mushroom,las=2)$out
for (i in 2:ncol(mushroom)) {
  mushroom[[i]] <- remove_outliers(mushroom[[i]])
}
mushroom$population<-remove_outliers(mushroom$population)
OutVals = boxplot(mushroom,las=2)$out



remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
#Draw data
library(ggplot2)
library(gridExtra)
myplots <- list()  # new empty list
for (i in 1:ncol(mushroom)) {
  p1 <- eval(substitute(
    ggplot(aes(x = mushroom[[i]]), data = mushroom) +
      geom_histogram(stat = "count") +
      facet_wrap(~class) +
      xlab(colnames(mushroom)[i]))
    ,list(i = i))
  myplots[[i]] <- p1  # add each plot into plot list
}#my precious
#we plot the histograms of each category and split them into two graphs according to their edibility.
grid.arrange(myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]], ncol = 2)
grid.arrange(myplots[[6]],myplots[[7]],myplots[[8]],myplots[[9]], ncol = 2)
grid.arrange(myplots[[10]],myplots[[11]],myplots[[12]],myplots[[13]], ncol = 2)
grid.arrange(myplots[[14]],myplots[[15]],myplots[[16]],myplots[[17]], ncol = 2)
grid.arrange(myplots[[18]],myplots[[19]],myplots[[20]],myplots[[21]],myplots[[22]],myplots[[23]], ncol = 2)
