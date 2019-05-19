source(file = "Preparation.R") # just shows worked dataset after data preparation.
#################################################### Virtualize the mushroom

library(ggplot2)
ggplot(mushroom, aes(x = cap_surface, y = cap_color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = cap_shape, y = cap_color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
ggplot(mushroom, aes(x = gill_color, y = cap_color, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

ggplot(mushroom, aes(x = class, y = odor, col = class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))

################################################### 

################################################### Modelling...




set.seed(579642)  #Set the seed for reproducibility
train_index <- sample(1:nrow(mushroom), size=nrow(mushroom)*0.8) # randomly choice rows
data_train  <- mushroom[train_index,]
data_test   <- mushroom[-train_index,]
trControl <- trainControl(method = "cv", number = 10, search = "grid")



############################# for ntree
## k fold cross validation takes too much time. So that, We optimize the 1-4 ntree.
start_time <- Sys.time()
ntreeAccuracy <- list()
for(i in c(1:20)){
  set.seed(579642)
  rf_ntree <- train(class~.,
                    data = data_train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl,
                    ntree = i)
  current_iteration <- toString(i)
  ntreeAccuracy[[current_iteration]] <- mean(rf_ntree$results$Accuracy)
}

print(ntreeAccuracy)


plot(unlist(ntreeAccuracy), type="o" , bty="l" , ylab="Accuracy" , xlab="ntreeValues" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )
meanNtreeAccuracy <- lapply(ntreeAccuracy, mean)
ntreeMax <- which.max(as.vector(unlist(meanNtreeAccuracy)))

############################# for mtry.

### mtry value continue as 1 after the 6.
tuneGrid <- expand.grid(.mtry = c(1: 6))
rf_mtry <- train(class~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 do.trace = TRUE, ## is given about of randomForest.
                 ntree = ntreeMax)
print(rf_mtry)
plot(rf_mtry, type="o" , bty="l" , ylab="Accuracy" , xlab="mtry Values" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )
rf_mtry$results$Accuracy
mean(rf_mtry$results$Accuracy)
tuneGrid <- expand.grid(.mtry = rf_mtry$bestTune$mtry )




############################# for maxnode

AccuracyList <- c()
RecallList <- c()
F_ScoreList <- c()
PrecisionList <- c()


store_maxnode <- list()
indexes <- c()
for (maxnodes in c(5: 15)) {
  set.seed(579642)
  rf_maxnode <- train(class~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      trControl = trControl,
                      
                      tuneGrid = tuneGrid,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = ntreeMax)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode$results$Accuracy
  indexes <- c(indexes,maxnodes)
  print(maxnodes)
  prediction <-predict(rf_maxnode, data_test)
  confusion_matrix <- confusionMatrix(prediction, data_test$class)
  confusion_matrix
  TP <- confusion_matrix$table[2,2]
  TN <- confusion_matrix$table[1,1]
  FP <- confusion_matrix$table[2,1]
  FN <- confusion_matrix$table[1,2]
  Accuracy <- (TP+TN)/(TP+TN+FP+FN)
  AccuracyList <- c(AccuracyList,Accuracy)
  
  Recall <- TP/(TP+FN)
  RecallList <- c(RecallList,Recall)
  
  F_Score <- 2*TP/(2*TP+FP+FN)
  F_ScoreList <- c(F_ScoreList , F_Score )
  
  Precision <- TP/(TP+FP)
  PrecisionList <- c(PrecisionList , Precision )
}
AccuracyList
RecallList 
F_ScoreList 
PrecisionList

plot(AccuracyList, type="o" , bty="l" , xlab="maxNodeValues" , ylab="Accuracy" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )
plot(RecallList, type="o" , bty="l" , xlab="maxNodeValues" , ylab="RecallList" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )
plot(F_ScoreList, type="o" , bty="l" , xlab="maxNodeValues" , ylab="F_ScoreList" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )
plot(PrecisionList, type="o" , bty="l" , xlab="maxNodeValues" , ylab="PrecisionList" , col=rgb(0.1,0.5,0.1,0.8) , lwd=0.5 , pch=16  )




mean(AccuracyList)
mean(RecallList)
mean(F_ScoreList)
mean(PrecisionList)



meanMaxnodesAccuracies <- lapply(store_maxnode, mean)
maxnodes <- which.max(as.vector(unlist(meanMaxnodesAccuracies)))
maxAccuracy <- max(as.vector(unlist(meanMaxnodesAccuracies)))
maxnodes <- indexes[maxnodes]

end_time <- Sys.time()

print(paste("running time => ", (end_time-start_time)))

## end.