#include data preparation file
source(file = "Preparation.R") # just shows worked dataset after data preparation.

#show current dataset
head(mushroom)
summary(mushroom)



#begin the logistic function (Model fitting)
set.seed(579642)  #Set the seed for reproducibility
train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*0.8) # randomly choice rows
test  <- mushroom[-train_index,]
train <- mushroom[train_index,]

library(arm)
#if we calculate the linear regression model, remove family part
try1 = bayesglm(formula = class ~ ., data = train, family=binomial(link='logit'))

glm.probs <- predict(try1, type = "response")
probs <- exp(glm.probs)/(1+exp(glm.probs)) #gives you probability that y=1 for each observation

glm.probs[1:5]
glm.pred  <- ifelse(glm.probs > 0.5,"p","e")
misClasificError <- mean(glm.pred != train$class)
print(paste('Accuracy',1-misClasificError))

#Assessing the predictive ability of the model
fitted.results <- predict(try1,newdata=subset(test),type='response')
fitted.results <- ifelse(fitted.results > 0.5,"p","e")

misClasificError <- mean(fitted.results != test$class)
print(paste('Accuracy',1-misClasificError))

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


