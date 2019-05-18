mushroom <- read.csv("agaricus-lepiota.csv",header = TRUE,na.strings=c("?","NA")) # load dataset

summary.default(mushroom)# data types


test<-sapply(mushroom, levels)
op2<-sapply(test,length)
barplot(op2,ylab="the number of factor levels", names.arg=names(mushroom),las=2)
    #show missing data in a table
library(Amelia)
any(is.na(mushroom))#missing değer var ise true döner
missmap(mushroom, main = "Missing values vs observed") # plot missing data distrubition
rm(i, sums, numMissData)


#Draw data
library(ggplot2)
library(gridExtra)
myplots <- list()  # new empty list
for (i in 1:ncol(mushroom)) {
  p1 <- eval(substitute(
    ggplot(aes(x = mushroom[[i]]), data = mushroom) +
      geom_histogram(stat = "count") +
     # facet_wrap(~class) +
      xlab(colnames(mushroom)[i]))
    ,list(i = i))
  myplots[[i]] <- p1  # add each plot into plot list
}
#we plot the histograms of each category
grid.arrange(myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]], ncol = 2)
grid.arrange(myplots[[6]],myplots[[7]],myplots[[8]],myplots[[9]], ncol = 2)
grid.arrange(myplots[[10]],myplots[[11]],myplots[[12]],myplots[[13]], ncol = 2)
grid.arrange(myplots[[14]],myplots[[15]],myplots[[16]],myplots[[17]], ncol = 2)
grid.arrange(myplots[[18]],myplots[[19]],myplots[[20]],myplots[[21]],myplots[[22]],myplots[[23]], ncol = 2)
      


#------------------------------Impotance level
ComputeProportion <- function(target,attribute_dataset,Columns,centroid){
  len_attr <- length(Columns)
  RMSE <- NULL
  for(i in 1:len_attr){
    tab <- table(target,attribute_dataset[,Columns[i]])
    prop <- tab[2,]/(tab[1,]+tab[2,])
    centr_prop <- prop - centroid
    Err <- sqrt(mean(centr_prop*centr_prop))
    RMSE <-rbind(RMSE,Err)
  }
  norm_err <- (RMSE-min(RMSE))/(max(RMSE)-min(RMSE))
  err_mat <- cbind(colnames(mushroom[,Columns]),RMSE)
  err_mat <- cbind(err_mat,norm_err)
  colnames(err_mat) <- c("Column name","RMSE","Norm. Error")
  err_mat
}

#-------------------Importance Levels
col<-c(2:23)
secondCol<-c(1:23)
secondCol<-secondCol[-8]
proportions_tab <- ComputeProportion(mushroom[,1],mushroom,col,0.482)


barplot(as.numeric(proportions_tab[,3]), ylab="Importance Level for edibility", 
       names.arg=proportions_tab[,1],
        border="blue", col=rainbow(3),las=2)
#for second target
proportions_tab <- ComputeProportion(mushroom[,8],mushroom,secondCol,0.161)
print(proportions_tab)
barplot(as.numeric(proportions_tab[,3]), ylab="Importance Level for gill spacing", 
        names.arg=proportions_tab[,1],
        border="blue", col=rainbow(3),las=2)

