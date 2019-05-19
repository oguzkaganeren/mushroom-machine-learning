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
#for second target
proportions_tab <- ComputeProportion(mushroom[,5],mushroom,secondCol,0.415)
print(proportions_tab)
#neden iki tanesini çıkardık(norm err. 0.5 altında kalıyor ikisinde de ondan dolayı)
#stalk_shape ve veil_type en düşük değere sahip(iki target içinde)
drops <- c("veil_type","stalk_shape") #there is one unique values of veil_type, stalk_shape we can remove this column in our dataset.
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type,stalk_shape
## We redefine each of the category for each of the variables
levels(mushroom$class) <- c("edible", "poisonous")
levels(mushroom$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroom$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroom$bruises) <- c("no", "yes")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gill_attachment) <- c("attached", "free")
levels(mushroom$gill_spacing) <- c("close", "crowded")
levels(mushroom$gill_size) <- c("broad", "narrow")
levels(mushroom$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ring_number) <- c("none", "one", "two")
levels(mushroom$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

# remove unncessary data
rm(drops)