#install.packages("Amelia")
#install.packages("caret")
library(arm)
library(caret)
library(purrr)
library(Amelia)
#include data preparation file
source(file = "Preparation.R") # just shows worked dataset after data preparation.

#----------------       
TRAIN_SIZE  = 0.8
NUM_OF_FOLD = 10
TRAIN_DATA  = list()

#show current dataset
#head(mushroom)
#summary(mushroom)

set.seed(5)  #Set the seed for reproducibility
for(i in 1:(NUM_OF_FOLD+1)){
  train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*TRAIN_SIZE) # randomly choice rows
  TRAIN_DATA <- c(TRAIN_DATA, list(train_index))
}

#begin the logistic function (Model fitting)
AccList <- NULL
RecList <- NULL
FscList <- NULL
PrcList <- NULL


for(i in 1:(NUM_OF_FOLD)){

  temp_index <- unlist(TRAIN_DATA[i])
  test  <- mushroom[-temp_index,]
  train <- mushroom[temp_index,]

start_time <- Sys.time()
model = glm(formula = gill_spacing ~ ., data = train, family = binomial())
#summary(model)
#NA değerler var, bunun sebebi birden fazla attribute un iyi bir şekilde eşleşmesi
#bu durumu istemiyoruz o yüzden NA'lı attributeları kaldırıyoruz
drops <- c("stalk_color_above_ring","stalk_color_below_ring","veil_color","ring_number"
           ,"ring_type","spore_print_color","habitat") #we remove this column in our dataset.
train <- train[ , !(names(train) %in% drops)] #remove
test <- test[ , !(names(test) %in% drops)] #remove
#Warning: glm.fit: algorithm did not converge hatası iterasyon sayısı ile ilgili
#default olarak maxit=25'dir biz 100 yapıyoruz
model <- glm(gill_spacing ~ ., data = train,family = binomial,maxit = 100)
#summary(model)

#şimdi modeli test datamız üzerinde test ediyoruz

predicted <- predict.glm(model,newdata = test[,-8],type = "response")
#tahmin edilen değerlerin 0.5 üzeri olanlar poison
predicted <- predicted >= 0.5
predicted <- gsub("FALSE","c",predicted)
predicted <- gsub("TRUE","w",predicted)

actual <- test[,8]

train_err <- mean(predicted != train$gill_size)
confusion_matrix <- table(predicted,actual)
TP <- confusion_matrix[2,2]
TN <- confusion_matrix[1,1]
FP <- confusion_matrix[2,1]
FN <- confusion_matrix[1,2]
Accuracy <- (TP+TN)*100/(TP+TN+FP+FN)
Recall <- TP*100/(TP+FN)
F_Score <- 2*TP*100/(2*TP+FP+FN)
Precision <- TP*100/(TP+FP)
end_time <- Sys.time()
AccList <- c(AccList, Accuracy)
RecList <- c(RecList, Recall)
FscList <- c(FscList, F_Score)
PrcList <- c(PrcList, Precision)

print(paste(i, "-> running time => ", (end_time-start_time), "Accuracy : ", Accuracy,
            " Recall : ", Recall
            ," F-Score : ", F_Score
            ," Precision : ", Precision))
}













#-------------------------cross class
TRAIN_SIZE  = 0.8 
NUM_OF_FOLD = 10
TRAIN_DATA  = list()

#begin the logistic function (Model fitting)
set.seed(48425)  #Set the seed for reproducibility

for(i in 1:(NUM_OF_FOLD+1)){
  train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*TRAIN_SIZE) # randomly choice rows
  TRAIN_DATA <- c(TRAIN_DATA, list(train_index))
}

AccList <- NULL
RecList <- NULL
FscList <- NULL
PrcList <- NULL


for(i in 1:(NUM_OF_FOLD)){
  temp_index <- unlist(TRAIN_DATA[i])
  test  <- mushroom[-temp_index,]
  train <- mushroom[temp_index,]
  
  start_time <- Sys.time()
  model = glm(formula = class ~ ., data = train, family = binomial())
  #summary(model)
  #NA değerler var, bunun sebebi birden fazla attribute un iyi bir şekilde eşleşmesi
  #bu durumu istemiyoruz o yüzden NA'lı attributeları kaldırıyoruz
  drops <- c("stalk_color_above_ring","stalk_color_below_ring","veil_color","ring_number"
             ,"ring_type","spore_print_color","habitat") #we remove this column in our dataset.
  
  train <- train[ , !(names(train) %in% drops)] #remove
  test <- test[ , !(names(test) %in% drops)] #remove
  #Warning: glm.fit: algorithm did not converge hatası iterasyon sayısı ile ilgili
  #default olarak maxit=25'dir biz 100 yapıyoruz
  model <- glm(class ~ ., data = train,family = binomial ,maxit = 100)
  #summary(model)
  
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
  Recall <- TP*100/(TP+FN)
  F_Score <- 2*TP*100/(2*TP+FP+FN)
  Precision <- TP*100/(TP+FP)
  end_time <- Sys.time()
  AccList <- c(AccList, Accuracy)
  RecList <- c(RecList, Recall)
  FscList <- c(FscList, F_Score)
  PrcList <- c(PrcList, Precision)
  
  print(paste(i, "-> running time => ", (end_time-start_time), "Accuracy : ", Accuracy,
              " Recall : ", Recall
              ," F-Score : ", F_Score
              ," Precision : ", Precision))
  
  
}

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
