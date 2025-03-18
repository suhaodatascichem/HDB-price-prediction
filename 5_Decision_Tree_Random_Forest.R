# install and load packages ----
install.packages("ROCR") 
install.packages("rpart") #Recursive Partitioning and Regression Trees
install.packages("randomForest")
install.packages("readxl")
install.packages("dplyr")
install.packages("caret")
library("caret")
library("dplyr")
library("readxl")
library("rpart")
library("ggplot2")
# read data file ----
setwd("C:\\Users\\Su Hao\\OneDrive - National University of Singapore\\General - MTech Y1S2\\Dataset")
 
seed <- 123 
set.seed(seed) 

dataset <- read_excel("resaleflats_merged_final_copy.xlsx")
colnames(dataset)
numRows <- nrow(dataset)  

# price is right skewed
ggplot(dataset,aes(resale_price_per_sqft)) + geom_histogram()
dataset$resale_price_per_sqft

# outlier values
outliers = filter(dataset, (abs(dataset$resale_price_per_sqft - median(dataset$resale_price_per_sqft)) > 2*sd(dataset$resale_price_per_sqft)))
dim(outliers)
### Remove outliers ----
df = filter(dataset, !(abs(resale_price_per_sqft - median(resale_price_per_sqft)) > 2*sd(resale_price_per_sqft)))
dim(df)

# "block" is not a good factor, I prefer use the distance 
df = df[, c("flat_type", "storey_range", "floor_area_sqft", "flat_model", "remaining_lease_mnths","distance_to_dhoby_ghaut",
                 "min_dist_prisch", "min_dist_rest", "min_dist_mall", "min_dist_spmkt", 
                 "rating_median", "vader_median", "dist_nearest_mrt", "resale_price_per_sqft", "time_diff")]
dim(df)

# collect the data indices returned in a list
inds = createDataPartition(df$resale_price_per_sqft, p = 0.7, list = F, times = 1)

## Generate Training Data ----
train_data = df[inds,]
nrow(train_data) / nrow(df)
dim(train_data)

## Generate Test Data ----
test_data = df[-inds,]
nrow(test_data) / nrow(df)
dim(test_data)

# Build the Decision Tree model ----
#take resale_price_per_sqft as the output and against selected variables as inputs
#select the dataset based on train index earlier
#method is "regression", i.e., not classification
#split criterion is by gini index, instead of information gain

DT <- rpart(`resale_price_per_sqft` ~ . ,
               data=train_data,  
               method="anova",
              parms=list(split="gini"),
            control=rpart.control(minsplit=20,
                                  minbucket=7,
                                  maxdepth=6
                                  )
           )

print(DT)

####### List the rules from the tree ----
list.rules.rpart <- function(DT)
{
  #check if it is rpart (DT) type, if not stop. 
  if (!inherits(DT, "rpart")) stop("Not a legitimate rpart tree")
  #
  # Get some information.
  #
  frm     <- DT$frame
  names   <- row.names(frm)
  ylevels <- attr(DT, "ylevels")
  ds.size <- DT$frame[1,]$n
  #
  # Print each leaf node as a rule.
  #
  for (i in 1:nrow(frm))
  {
    if (frm[i,1] == "<leaf>")
    {
      cat("\n")
      cat(sprintf(" Rule number: %s ", names[i]))
      cat(sprintf("[yval=%s cover=%d (%.0f%%) prob=%0.2f]\n",
                  ylevels[frm[i,]$yval], frm[i,]$n,
                  round(100*frm[i,]$n/ds.size), frm[i,]$yval2[,5]))
      pth <- path.rpart(DT, nodes=as.numeric(names[i]), print.it=FALSE)
      cat(sprintf("   %s\n", unlist(pth)[-1]), sep="")
    }
  }
}
list.rules.rpart(DT)

# Plot the resulting Decision Tree. 
library(rpart.plot)
rpart.plot(DT,cex=0.7)

######### DT Train Accuracy
# Obtain the prediction from the Decision Tree model.
DT_TrainPredict <- predict(DT, newdata=train_data)

# calculate SSE, MSE, RMSE

#DT_TrainCM<-table(train_data$`resale_price_per_sqft`, DT_TrainPredict,
#                 dnn=c("Actual", "Predicted"))
#DT_TrainCM

######### DT Test Accuracy  

# Obtain the prediction from the Decision Tree model.
DT_TestPredict <- predict(DT, newdata=test_data)
DT_TestPredict
### training set accuracy
library(caret)

# Calculate Mean Absolute Error (MAE)
mae_DT_train <- mean(abs(train_data$`resale_price_per_sqft` -  DT_TrainPredict))

# Calculate Mean Squared Error (MSE)
mse_DT_train <- mean((train_data$`resale_price_per_sqft` -  DT_TrainPredict)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse_DT_train <- sqrt(mse_DT_train)

# Calculate R-squared (R^2)
R2_DT_train <- 1 - (sum((train_data$`resale_price_per_sqft` -  DT_TrainPredict)^2) / sum((train_data$`resale_price_per_sqft` - mean(train_data$`resale_price_per_sqft`))^2))

# Print the results
cat("Train set Mean Absolute Error (MAE):", mae_DT_train, "\n")
cat("Train setMean Squared Error (MSE):", mse_DT_train, "\n")
cat("Train setRoot Mean Squared Error (RMSE):", rmse_DT_train, "\n")
cat("Train setR-squared (R^2):", R2_DT_train, "\n")

### test set accuracy

# Calculate Mean Absolute Error (MAE)
mae_DT_test <- mean(abs(test_data$`resale_price_per_sqft` -  DT_TestPredict))

# Calculate Mean Squared Error (MSE)
mse_DT_test <- mean((test_data$`resale_price_per_sqft` -  DT_TestPredict)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse_DT_test <- sqrt(mse_DT_test)

# Calculate R-squared (R^2)
R2_DT_test <- 1 - (sum((test_data$`resale_price_per_sqft` -  DT_TestPredict)^2) / sum((test_data$`resale_price_per_sqft` - mean(test_data$`resale_price_per_sqft`))^2))

# Print the results
cat("Test set Mean Absolute Error (MAE):", mae_DT_test, "\n")
cat("Test set Mean Squared Error (MSE):", mse_DT_test, "\n")
cat("Test set Root Mean Squared Error (RMSE):", rmse_DT_test, "\n")
cat("Test set R-squared (R^2):", R2_DT_test, "\n")


#####################  Random Forest model #################################
############################################################################


library(randomForest)

RF <- randomForest(resale_price_per_sqft ~ .,
                  data=train_data,
                  )

#ntree = number of DT
#mtry = number of features to try each time

importance(RF)
varImpPlot(RF)

# Obtain the prediction from the random forest model.
RF_TrainPredict <- predict(RF, newdata=train_data)

RF_TestPredict <- predict(RF, newdata=test_data)
RF_TestPredict
library(caret)
# RF Train Accuracy 

# Calculate Mean Absolute Error (MAE)
mae_RF_train <- mean(abs(train_data$`resale_price_per_sqft` -  RF_TrainPredict))

# Calculate Mean Squared Error (MSE)
mse_RF_train <- mean((train_data$`resale_price_per_sqft` -  RF_TrainPredict)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse_RF_train <- sqrt(mse_RF_train)

# Calculate R-squared (R^2)
R2_RF_train <- 1 - (sum((train_data$`resale_price_per_sqft` -  RF_TrainPredict)^2) / sum((train_data$`resale_price_per_sqft` - mean(train_data$`resale_price_per_sqft`))^2))

# Print the results
cat("Train set Mean Absolute Error (MAE):", mae_RF_train, "\n")
cat("Train setMean Squared Error (MSE):", mse_RF_train, "\n")
cat("Train setRoot Mean Squared Error (RMSE):", rmse_RF_train, "\n")
cat("Train setR-squared (R^2):", R2_RF_train, "\n")

### test set accuracy

# Calculate Mean Absolute Error (MAE)
mae_RF_test <- mean(abs(test_data$`resale_price_per_sqft` -  RF_TestPredict))

# Calculate Mean Squared Error (MSE)
mse_RF_test <- mean((test_data$`resale_price_per_sqft` -  RF_TestPredict)^2)

# Calculate Root Mean Squared Error (RMSE)
rmse_RF_test <- sqrt(mse_RF_test)

# Calculate R-squared (R^2)
R2_RF_test <- 1 - (sum((test_data$`resale_price_per_sqft` -  RF_TestPredict)^2) / sum((test_data$`resale_price_per_sqft` - mean(test_data$`resale_price_per_sqft`))^2))

# Print the results
cat("Test set Mean Absolute Error (MAE):", mae_RF_test, "\n")
cat("Test set Mean Squared Error (MSE):", mse_RF_test, "\n")
cat("Test set Root Mean Squared Error (RMSE):", rmse_RF_test, "\n")
cat("Test set R-squared (R^2):", R2_RF_test, "\n")