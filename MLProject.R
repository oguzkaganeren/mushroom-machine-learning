#install.packages("Amelia")
#install.packages("caret")
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
set.seed(48425)  #Set the seed for reproducibility

train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*TRAIN_SIZE) # randomly choice rows
test  <- mushroom[-train_index,]
train <- mushroom[train_index,]

start_time <- Sys.time()
model = glm(formula = class ~ ., data = train, family = binomial())
summary(model)
#NA değerler var, bunun sebebi birden fazla attribute un iyi bir şekilde eşleşmesi
#bu durumu istemiyoruz o yüzden NA'lı attributeları kaldırıyoruz
drops <- c("stalk_color_above_ring","stalk_color_below_ring","veil_color","ring_number"
           ,"ring_type","spore_print_color","habitat") #we remove this column in our dataset.
train <- train[ , !(names(train) %in% drops)] #remove
test <- test[ , !(names(test) %in% drops)] #remove
#Warning: glm.fit: algorithm did not converge hatası iterasyon sayısı ile ilgili
#default olarak maxit=25'dir biz 100 yapıyoruz
model <- glm(class ~ ., data = train,family = binomial,maxit = 100)
summary(model)

#şimdi modeli test datamız üzerinde test ediyoruz
predicted <- predict.glm(model,newdata = test[,-1],type = "response")
#tahmin edilen değerlerin 0.5 üzeri olanlar poison
predicted <- predicted >= 0.5
predicted <- gsub("TRUE","p",predicted)
predicted <- gsub("FALSE","e",predicted)

actual <- test[,1]

train_err <- mean(predicted != train$class)
confusion_matrix <- table(predicted,actual)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[2,1]
FN <- confusion_matrix[1,2]
Accuracy <- (TP+TN)*100/(TP+TN+FP+FN)
print(Accuracy)

Recall <- TP*100/(TP+FN)
print(Recall)

F_Score <- 2*TP*100/(2*TP+FP+FN)
print(F_Score)

Precision <- TP*100/(TP+FP)
print(Precision)

end_time <- Sys.time()

print(paste("running time => ", (end_time-start_time)))





  

# crossvalidation ? does not work
#train_control = trainControl(method="repeatedcv", number=5, repeats=5)
#modelLog = train(formula = class ~ ., data=train, method="glm", family=binomial, trControl=train_control)
#PredTrain = predict(modelLog, newdata=train, type="raw") 
#table(train$class, PredTrain > 0.5)



#ROC Curve
#install.packages("ROCR")
#library(ROCR)
#p <- predict(model, newdata=subset(test), type="response")
#pr <- prediction(p, test$class)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc
#https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/


