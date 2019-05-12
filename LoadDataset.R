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