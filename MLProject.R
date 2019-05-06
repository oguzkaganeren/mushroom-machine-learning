#include data preparation file
source(file = "Preparation.R") # just shows worked dataset after data preparation.

TRAIN_SIZE  = 0.8 
NUM_OF_FOLD = 10
TRAIN_DATA  = list()

library(arm)
#install.packages("caret")
library(caret)
#show current dataset
#head(mushroom)
#summary(mushroom)

#begin the logistic function (Model fitting)
set.seed(1579642)  #Set the seed for reproducibility
for(i in 1:(NUM_OF_FOLD+1)){
  train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*TRAIN_SIZE) # randomly choice rows
  TRAIN_DATA <- c(TRAIN_DATA, list(train_index))
}


for(i in 1:(NUM_OF_FOLD+1)){
    temp_index <- unlist(TRAIN_DATA[i])
    test  <- mushroom[-temp_index,]
    train <- mushroom[temp_index,]
    
    
    model = bayesglm(formula = class ~ ., data = train, family=binomial(link = "logit"))
    glm.pred  <- ifelse(predict(model, type = "response") > 0.5,"p","e")
    train_err <- mean(glm.pred != train$class)
    
    #Assessing the predictive ability of the model
    fitted.results <- predict(model,newdata=subset(test),type='response')
    fitted.results <- ifelse(fitted.results > 0.5,"p","e")
    test_err <- mean(fitted.results != test$class)
    
    print(paste(i, '-> Train : Accuracy',1-train_err,'Test : Accuracy',1-test_err))
    
  
}

# crossvalidation ? does not work
train_control = trainControl(method="repeatedcv", number=5, repeats=5)
modelLog = train(formula = class ~ ., data=train, method="glm", family=binomial, trControl=train_control)
PredTrain = predict(modelLog, newdata=train, type="raw") 
table(train$class, PredTrain > 0.5)


"
family(object, …)
binomial(link = "logit")
gaussian(link = "identity")
Gamma(link = "inverse")
inverse.gaussian(link = "1/mu^2")
poisson(link = "log")
quasi(link = "identity", variance = "constant")
quasibinomial(link = "logit")
quasipoisson(link = "log")
"


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


