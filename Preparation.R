#install.packages("Amelia")
#library(Amelia)
columnNames <- c(
  "class", "cap_shape", "cap_surface", 
  "cap_color", "bruises", "odor", 
  "gill_attachement", "gill_spacing", "gill_size", 
  "gill_color", "stalk_shape", "stalk_root", 
  "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
  "stalk_color_below_ring", "veil_type", "veil_color", 
  "ring_number", "ring_type", "spore_print_color", 
  "population", "habitat")

#include dataset from disk
mushroom <- read.table("agaricus-lepiota.data",
                       sep = ",",
                       na.strings = "?",
                       colClasses = NA,
                       header = FALSE,
                       col.names= columnNames
)


#missmap(mushroom, main = "Missing values vs observed")

#replace NA values to columns mode
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

for (var in 1:ncol(mushroom)) {
  mushroom[is.na(mushroom[,var]),var] <- Mode(mushroom[,var], na.rm = TRUE)
}

#Analysis

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

#after data preparation of missing values.
#missmap(mushroom, main = "After data preparation of missing values")


drops <- c("veil_type") #there is one unique values of veil_type, we can remove this column in our dataset.
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type
#head(mushroom) # without numeric values, pure non preparing

## categoric to numeric without target
for (var in 1:ncol(mushroom)) {
  mushroom[[var]] <- as.numeric( mushroom[[var]])
}


#After the mode process, the graph

#head(mushroom)

#head(mushroom) # list the dataset after fill missing values, categoricial to numericial, and normalization.
# remove unncessary data
rm(columnNames,drops,var,Mode)

