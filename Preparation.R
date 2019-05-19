mushroom <- read.csv("agaricus-lepiota.csv",header = TRUE,na.strings=c("?","NA")) # load dataset

#replace NA values to columns mode  
Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for (var in 1:ncol(mushroom)) {
  mushroom[is.na(mushroom[,var]),var] <- Mode(mushroom[,var], na.rm = TRUE)
}

#--------------------
#doğal değerlerini buluyoruz proportion kısmında lazım olacak
class_count <- table(mushroom$class)
class_proportion <- class_count[2]/(class_count[1]+class_count[2])
print("Natural poison rate->")
class_proportion


class_count <- table(mushroom$gill_spacing)
class_proportion <- class_count[2]/(class_count[1]+class_count[2])
print("Natural buises rate->")
class_proportion



#dönen değer ne kadar büyükse sınıflandırma için o kadar önemlidir.
#columns target dışında yer alan diğer attributesları alır
#parametre olarak verilen centroid değeri dogal posion değeridir
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
col<-c(2:23)
secondCol<-c(1:23)
secondCol<-secondCol[-4]
proportions_tab <- ComputeProportion(mushroom[,1],mushroom,col,0.482)
print(proportions_tab)
#neden iki tanesini çıkardık(norm err. 0.5 altında kalıyor ikisinde de ondan dolayı)
#NA değerler var, bunun sebebi birden fazla attribute un iyi bir şekilde eşleşmesi
#bu durumu istemiyoruz o yüzden NA'lı attributeları kaldırıyoruz
drops <- c("veil_type","stalk_shape", "stalk_color_above_ring","stalk_color_below_ring","veil_color","ring_number"
           ,"ring_type","spore_print_color","habitat") #we remove this column in our dataset.
#stalk_shape ve veil_type en düşük değere sahip(iki target içinde)
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type,stalk_shape

# remove unncessary data
rm(drops,proportions_tab, class_count, class_proportion, col, secondCol, var, ComputeProportion, Mode)
