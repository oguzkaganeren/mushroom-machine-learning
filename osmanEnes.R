library(tidyverse)
url_file <-  "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroom <- read.csv(url(url_file), header=FALSE)
glimpse(mushroom)

colNames <- c("class", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")
colnames(mushroom) <- colNames
mushroom <- mushroom %>% map_df(function(.x) as.factor(.x))

## We redefine each of the category for each of the variables
levels(mushroom$class) <- c("edible", "poisonous")
levels(mushroom$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroom$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroom$bruises) <- c("no", "yes")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gill_attachement) <- c("attached", "free")
levels(mushroom$gill_spacing) <- c("close", "crowded")
levels(mushroom$gill_size) <- c("broad", "narrow")
levels(mushroom$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalk_shape) <- c("enlarging", "tapering")
levels(mushroom$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$veil_type) <- "partial" ## one attribute this column will be deleted.
levels(mushroom$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ring_number) <- c("none", "one", "two")
levels(mushroom$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

glimpse(mushroom)



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


