# 1. Start SVM Analysis ------------------------------------------------------------
#Utility Functions included
source("https://raw.githubusercontent.com/mk01git/r-predict-sales-volume/master/Utility.R")

# 2. Load Section ------------------------------------------------------------
# Load new data from github URL for analysis
new.data <- read.csv(paste("https://raw.githubusercontent.com/mk01git/r-predict-sales-volume/master/new-products.csv", sep=","), header=TRUE)
# Load existing data from github URL for analysis
existing.data <- read.csv(paste("https://raw.githubusercontent.com/mk01git/r-predict-sales-volume/master/existing-products.csv", sep=","), header = TRUE)

# 3. Understanding Data Section ---------------------------------------------------------
understandDataFrame(new.data)
understandDataFrame(existing.data)

# 4. Data cleanup Section ---------------------------------------------------------
#rename existing columns in datasets
new.data <- renamecolumnsuserfriendly(new.data)
existing.data <- renamecolumnsuserfriendly(existing.data)


# 5. Data Preprocess Section ---------------------------------------------------------
#Preprocess the existing.data
existing.data$"Product.Type" <- NULL
existing.data$"Best.Sellers.Rank" <- NULL
existing.data$"Product.Width" <- NULL
existing.data$"Product.Height" <- NULL
existing.data$"Product.Depth" <- NULL
existing.data$"Would.consumer.recommend.product" <- NULL

#Preprocess the new.data
new.data$"Product.Type" <- NULL
new.data$"Best.Sellers.Rank" <- NULL
new.data$"Product.Width" <- NULL
new.data$"Product.Height" <- NULL
new.data$"Product.Depth" <- NULL
new.data$"Would.consumer.recommend.product" <- NULL

# 6. Install Package Section -----------------------------------------------------
#check and make sure e1071 package is installed.
if(!require("e1071")){
  print("e1071 package is not installed")
  install.packages("e1071")
}
#use e1071 library
library("e1071")


# 7. Create Training and Test Set Section -----------------------------------------------------
# Split dataset into 80% for training and remaining 20% for testing.
indices <- sample(2, nrow(existing.data), replace = TRUE, prob = c(0.8, 0.2))
trainset <- existing.data[indices==1,]
testset <- existing.data[indices==2,]

# 8. Create Model Section -----------------------------------------------------
#SVM - Model
?svm
model <- svm(trainset$Volume ~ . , trainset, kernel="polynomial", cost=4, nu=0.10, gamma=10, type='nu-classification', probability=TRUE)
summary(model)

#TestSet predictions
testprediction <- predict(model, testset, interval="predict", level=0.95)
testprediction

# 9. Run New Data Prediction Section -----------------------------------------------------
#Run prediction 
newPrediction <- cbind(new.data, newpred = predict(model, new.data))
newPrediction

# 10. Save Output Section ----------------------------------------------------
#Save result data to csv file.
write.csv(newPrediction, "SVM-Result.csv")


# 11. End Analysis ----------------------------------------------------
