mushroom <- read.csv("agaricus-lepiota.csv",header = TRUE) # load dataset
#replace NA values to columns mode  
#Mode <- function (x, na.rm) {
#  xtab <- table(x)
#  xmode <- names(which(xtab == max(xtab)))
#  if (length(xmode) > 1) xmode <- ">1 mode"
#  return(xmode)
#}

#for (var in 1:ncol(mushroom)) {
#  mushroom[is.na(mushroom[,var]),var] <- Mode(mushroom[,var], na.rm = TRUE)
#}

#after data preparation of missing values.
#missmap(mushroom, main = "After data preparation of missing values")
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

proportions_tab <- ComputeProportion(mushroom[,1],mushroom,c(2:23),0.482)
print(proportions_tab)

#stalk_shape ve veil_type en düşük değere sahip
drops <- c("veil_type","stalk_shape") #there is one unique values of veil_type, stalk_shape we can remove this column in our dataset.
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type,stalk_shape


# remove unncessary data
rm(drops)
