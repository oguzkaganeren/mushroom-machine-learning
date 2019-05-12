source(file = "LoadDataset.R") # load dataset
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
plot(mushroom$cap_shape, mushroom$class)
