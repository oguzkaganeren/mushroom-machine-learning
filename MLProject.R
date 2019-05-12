#install.packages("Amelia")
#install.packages("caret")
library(robustHD) # standardize function
library(arm)
library(caret)
library(purrr)
library(Amelia)
#include data preparation file
source(file = "Preparation.R") # just shows worked dataset after data preparation.

TRAIN_SIZE  = 0.8 
#NUM_OF_FOLD = 10
#TRAIN_DATA  = list()

#show current dataset
#head(mushroom)
#summary(mushroom)

#begin the logistic function (Model fitting)
set.seed(1579642)  #Set the seed for reproducibility

train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*TRAIN_SIZE) # randomly choice rows
test  <- mushroom[-train_index,]
train <- mushroom[train_index,]

start_time <- Sys.time()
model = glm(formula = class ~ ., data = train, family = binomial())
#anova(model)
glm.pred  <- ifelse(predict(model, type = "response") > 0.5,"p","e")
train_err <- mean(glm.pred != train$class)

#Assessing the predictive ability of the model
fitted.results <- predict(model,newdata=subset(test),type='response')
fitted.results <- ifelse(fitted.results > 0.5,"p","e")
test_err <- mean(fitted.results != test$class)

print(paste('Train : Accuracy',1-train_err,'Test : Accuracy',1-test_err))
end_time <- Sys.time()

print(paste("running time => ", (end_time-start_time)))







# crossvalidation ? does not work
train_control = trainControl(method="repeatedcv", number=5, repeats=5)
modelLog = train(formula = class ~ ., data=train, method="glm", family=binomial, trControl=train_control)
PredTrain = predict(modelLog, newdata=train, type="raw") 
table(train$class, PredTrain > 0.5)



#ROC Curve
#install.packages("ROCR")
library(ROCR)
p <- predict(try1, newdata=subset(test), type="response")
pr <- prediction(p, test$class)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


