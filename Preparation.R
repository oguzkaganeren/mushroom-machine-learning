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
) # there are missing value so it gets warning

#unique(mushroom$veil_type)

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

#after data preparation of missing values.
#missmap(mushroom, main = "After data preparation of missing values")

drops <- c("veil_type") #there is one unique values of veil_type, we can remove this column in our dataset.
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type
#head(mushroom) # without numeric values, pure non preparing

## categoric to numeric without target
library(mlr)
mush <- createDummyFeatures(mushroom[2:22])
mush$class <- mushroom$class
#After the mode process, the graph

head(mush)

#head(mushroom) # list the dataset after fill missing values, categoricial to numericial, and normalization.
# remove unncessary data
rm(columnNames,drops,var,Mode, mushroom)

