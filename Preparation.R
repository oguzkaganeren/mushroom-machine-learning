source(file = "LoadDataset.R") # load dataset
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

#after data preparation of missing values.
#missmap(mushroom, main = "After data preparation of missing values")

#unique(mushroom$veil_type) # there is an unique value of veil_type column.
drops <- c("veil_type") #there is one unique values of veil_type, we can remove this column in our dataset.
mushroom <- mushroom[ , !(names(mushroom) %in% drops)] #remove veil_type
#head(mushroom) # without numeric values, pure non preparing

## categoric to numeric without target

for (var in 2:ncol(mushroom)) {
  mushroom[[var]] <- as.integer( mushroom[[var]])
}

#normalize integer values without class variable by robustHD library
#mushroom[,2:22] <-  standardize(mushroom[,2:22])

"for (var in 1:ncol(mushroom)) {
  mushroom[[var]] <- as.factor( mushroom[[var]])
}"

#head(mushroom) # list the dataset after fill missing values, categoricial to numericial, and normalization.
# remove unncessary data
rm(columnNames,drops,var,Mode)
