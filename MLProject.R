
columnNames <- c(
  "edibility", "cap_shape", "cap_surface", 
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

head(mushroom) # without numeric values, pure non preparing

#After the mode process, the graph
missmap(mushroom, main = "Missing values vs observed")
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

#install.packages("corrplot")
library(corrplot)
correlations <- cor(mushroom[,2:22])
corrplot(correlations, method="circle")
#A dot-representation was used where blue represents positive correlation and red negative.
#The larger the dot the larger the correlation.
#You can see that the matrix is symmetrical and that the diagonal are perfectly positively correlated because it 
#shows the correlation of each variable with itself. Unfortunately, none of the variables are correlated with one another.


#pairs
pairs(mushroom, col=mushroom$edibility)#take a long time



# Logistics Regression training and test
library(caret)
inTrain <- createDataPartition(y = mushroom$edibility, p = .80, list = FALSE)
training_mushroom <- mushroom[inTrain,]# %80
test_mushroom <- mushroom[-inTrain,] # %20

summary(mushroom)



#there is a problem to solve
glm.fit <- glm(edibility ~ cap_shape + cap_surface + cap_color + bruises + odor + 
               gill_attachement + gill_spacing + gill_size + 
               gill_color + stalk_shape + stalk_root + 
               stalk_surface_above_ring + stalk_surface_below_ring + stalk_color_above_ring + 
               stalk_color_below_ring + 
               ring_number + spore_print_color +
               population + habitat,
               data = training_mushroom, 
               family = binomial)#missing some columns

summary(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")

sum(is.na(mushroom$veil_type))   
unique(mushroom$veil_type)
#there is one unique values of veil_type, we can remove this column in our dataset.
mushroom <- subset(mushroom, select = -c(17)) #17 is index of veil_type
head(mushroom)

#normalize integer values
normFunc <- function(x){(as.integer(x)-mean(as.integer(x), na.rm = T))/sd(as.integer(x), na.rm = T)}
mushroom[2:22] <- apply(mushroom[2:22], 2, normFunc)

head(mushroom)

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