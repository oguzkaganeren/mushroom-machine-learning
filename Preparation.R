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
mushroom$cap_shape <- scale(as.numeric(mushroom$cap_shape), center = TRUE, scale = TRUE)
mushroom$cap_surface <- scale(as.numeric(mushroom$cap_surface), center = TRUE, scale = TRUE)
mushroom$cap_color <- scale(as.numeric(mushroom$cap_color), center = TRUE, scale = TRUE)
mushroom$bruises <- scale(as.numeric(mushroom$bruises), center = TRUE, scale = TRUE)
mushroom$odor <- scale(as.numeric(mushroom$odor), center = TRUE, scale = TRUE)
mushroom$gill_attachement <-scale(as.numeric(mushroom$gill_attachement), center = TRUE, scale = TRUE)
mushroom$gill_spacing <- scale(as.numeric(mushroom$gill_spacing), center = TRUE, scale = TRUE)
mushroom$gill_size <- scale(as.numeric(mushroom$gill_size), center = TRUE, scale = TRUE)
mushroom$gill_color <- scale(as.numeric(mushroom$gill_color), center = TRUE, scale = TRUE)
mushroom$stalk_shape <- scale(as.numeric(mushroom$stalk_shape), center = TRUE, scale = TRUE)
mushroom$stalk_root <- scale(as.numeric(mushroom$stalk_root), center = TRUE, scale = TRUE)
mushroom$stalk_surface_above_ring <- scale(as.numeric(mushroom$stalk_surface_above_ring), center = TRUE, scale = TRUE)
mushroom$stalk_surface_below_ring <- scale(as.numeric(mushroom$stalk_surface_below_ring), center = TRUE, scale = TRUE)
mushroom$stalk_color_above_ring <- scale(as.numeric(mushroom$stalk_color_above_ring), center = TRUE, scale = TRUE)
mushroom$stalk_color_below_ring <- scale(as.numeric(mushroom$stalk_color_below_ring), center = TRUE, scale = TRUE)
mushroom$veil_color <- scale(as.numeric(mushroom$veil_color), center = TRUE, scale = TRUE)
mushroom$ring_number <- scale(as.numeric(mushroom$ring_number), center = TRUE, scale = TRUE)
mushroom$ring_type <- scale(as.numeric(mushroom$ring_type), center = TRUE, scale = TRUE)
mushroom$spore_print_color <- scale(as.numeric(mushroom$spore_print_color), center = TRUE, scale = TRUE)
mushroom$population <- scale(as.numeric(mushroom$population), center = TRUE, scale = TRUE)
mushroom$habitat <- scale(as.numeric(mushroom$habitat), center = TRUE, scale = TRUE)


#After the mode process, the graph

#head(mushroom)

#head(mushroom) # list the dataset after fill missing values, categoricial to numericial, and normalization.
# remove unncessary data
rm(columnNames,drops,var,Mode)

