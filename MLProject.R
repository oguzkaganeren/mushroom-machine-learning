#library(tidyverse)
#install.packages("tidyverse")
mushroom <- read.table("agaricus-lepiota.data",
                       sep = ",",
                       na.strings = "?",
                       colClasses = NA,
                       header = FALSE,
                       col.names=c(
                         "edibility", "cap_shape", "cap_surface", 
                         "cap_color", "bruises", "odor", 
                         "gill_attachement", "gill_spacing", "gill_size", 
                         "gill_color", "stalk_shape", "stalk_root", 
                         "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                         "stalk_color_below_ring", "veil_type", "veil_color", 
                         "ring_number", "ring_type", "spore_print_color", 
                         "population", "habitat"
                       )) # there are missing value so it gets warning

## categoric to numeric without target
mushroom$cap_shape <- as.numeric(mushroom$cap_shape)
mushroom$cap_surface <- as.numeric(mushroom$cap_surface)
mushroom$cap_color <- as.numeric(mushroom$cap_color)
mushroom$bruises <- as.numeric(mushroom$bruises)
mushroom$odor <- as.numeric(mushroom$odor)
mushroom$gill_attachement <- as.numeric(mushroom$gill_attachement)
mushroom$gill_spacing <- as.numeric(mushroom$gill_spacing)
mushroom$gill_size <- as.numeric(mushroom$gill_size)
mushroom$gill_color <- as.numeric(mushroom$gill_color)
mushroom$stalk_shape <- as.numeric(mushroom$stalk_shape)
mushroom$stalk_root <- as.numeric(mushroom$stalk_root)
mushroom$stalk_surface_above_ring <- as.numeric(mushroom$cap_shape)
mushroom$stalk_surface_below_ring <- as.numeric(mushroom$cap_shape)
mushroom$stalk_color_above_ring <- as.numeric(mushroom$stalk_color_above_ring)
mushroom$stalk_color_below_ring <- as.numeric(mushroom$stalk_color_below_ring)
mushroom$veil_type <- as.numeric(mushroom$veil_type)
mushroom$veil_color <- as.numeric(mushroom$veil_color)
mushroom$ring_number <- as.numeric(mushroom$ring_number)
mushroom$ring_type <- as.numeric(mushroom$ring_type)
mushroom$spore_print_color <- as.numeric(mushroom$spore_print_color)
mushroom$population <- as.numeric(mushroom$population)
mushroom$habitat <- as.numeric(mushroom$habitat)

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

sum(is.na(mushroom$stalk_root))   
library(ggplot2)
ggplot(mushroom, aes(x = cap_surface, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = cap_shape, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = gill_color, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = edibility, y = odor, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

