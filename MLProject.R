#include data preparation file
source(file = "Preparation.R") # just shows worked dataset after data preparation.

#show current dataset
head(dataset)
summary(dataset)

#begin the logistic function
set.seed(579642)  #Set the seed for reproducibility
train_index <- sample(1:nrow(dataset), size=nrow(dataset)*0.8) # randomly choice rows
test  <- dataset[-train_index,]
train <- dataset[train_index,]

try1 = glm(formula = class ~ ., data = dataset, family = "binomial")
summary(try1)
display(try1, digits = 4)
help(glm)
