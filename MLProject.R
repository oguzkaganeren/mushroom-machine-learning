#include data preparation file
source(file = "Preparation.R")

count(is.na(mush))
#show current dataset
head(mush)
summary(mush)

#begin the logistic function
set.seed(579642)  #Set the seed for reproducibility
train_index <- sample(1:nrow(mush), size=nrow(mush)*0.8) # randomly choice rows
test  <- mush[-train_index,]
train <- mush[train_index,]

try1 = glm(formula = class ~ ., data = mush, subset = train, family = "binomial")
summary(try1)
display(try1, digits = 4)
help(glm)
