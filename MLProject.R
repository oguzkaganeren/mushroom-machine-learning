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
#mushroom <- read_csv("agaricus-lepiota.data", col_names = FALSE) # there are missing value so it gets warning
#glimpse(mushroom) # This is like a transposed version of print()
# Rename the variables
#colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
#                        "cap_color", "bruises", "odor", 
#                        "gill_attachement", "gill_spacing", "gill_size", 
#                        "gill_color", "stalk_shape", "stalk_root", 
#                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
#                        "stalk_color_below_ring", "veil_type", "veil_color", 
#                        "ring_number", "ring_type", "spore_print_color", 
#                        "population", "habitat")

# Defining the levels for the categorical variables 
## We make each variable as a factor
#mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))

mushroom$stalk_root <- as.numeric (mushroom$stalk_root)
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

