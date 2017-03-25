
# function used to column names user friendly names.
renamecolumnsuserfriendly <- function(df){
  
  renamed.columns <- grep("(^X$)|(^X\\.)", colnames(df), perl=T)
  if (length(renamed.columns) > 0) {
    colnames(df) <- gsub("^X.", "",  colnames(df))
    colnames(df) <- gsub("(\\.$)+", "", colnames(df))
    colnames(df) <- gsub("(\\..$)+", "", colnames(df))
    colnames(df)[which(colnames(df) == 'Product')] <- 'Product.Number'
    colnames(df)[which(colnames(df) == '5.Star.Reviews')] <- 'Five.Star.Reviews'
    colnames(df)[which(colnames(df) == '4.Star.Reviews')] <- 'Four.Star.Reviews'
    colnames(df)[which(colnames(df) == '3.Star.Reviews')] <- 'Three.Star.Reviews'
    colnames(df)[which(colnames(df) == '2.Star.Reviews')] <- 'Two.Star.Reviews'
    colnames(df)[which(colnames(df) == '1.Star.Reviews')] <- 'One.Star.Reviews'
    colnames(df)[which(colnames(df) == 'Shipping.Weight..lbs.')] <- 'Shipping.Weight.lbs'
  }
  return(df)
}


understandDataFrame <- function(){
  #display data frame 
  head(new.data)
  head(existing.data)
  
  #display names for the columns available in dataset
  names(new.data)
  colnames(new.data)
  
  #Display number of columns
  ncol(existing.data)
  
  #Rows in the dataset
  nrow(new.data)
  nrow(existing.data)
  
  #count missing values in the dataset
  sum(is.na(new.data))
  
  #exclude the missing values rows 
  na.omit(new.data)
  
  #display each column data summary, such as Min, Max, Mean, Median, 1st Qu, 3rd QU..
  summary(new.data)
  
  #display number of rows and columns
  dim(new.data)
  #display first two rows from the dataset
  head(new.data, n = 2)
  
  #display last two frows from the dataset
  tail(new.data, n = 2)
  
  #display internal structure of the dataset
  str(existing.data)
  
  #display mean value for the Volumn column
  mean(existing.data$Volume)
  
  #display type of the column
  class(existing.data$Volume)

}