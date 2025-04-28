source("assignment-data-generator.R")
library("xgboost")


model_dev <- function(df, flag=1){
  #This is the model development function, which will take in different set of data set and divide that into train and test set and then call the "Xgboost_simple_cv" with the application of simple cross validation on "nrounds" parameter of XGBoost.
  
  set.seed(100) #Setting seed as 100
  train_index <- caret::createDataPartition(df$outcome, p=0.8, list=FALSE) #Using the createDataPartition function from caret package
  train_data <- df[train_index,]
  test_data <- df[-train_index,]
  X_train <- as.matrix(train_data[,-9])
  X_test <- as.matrix(test_data[,-9])
  y_train <- train_data$outcome
  y_test <- test_data$outcome
  #With the X_train and y_train, we will create DMatrix to use for XGBoost
  DMtrain <- xgb.DMatrix(data=X_train, label=y_train)
  
  if (flag==1){
    maxrounds<- 5 #Using this to iterate and model different XGboost model
    errvals <- map_dbl(1:maxrounds, ~Xgboost_simple_cv(DMtrain, X_test, y_test, .x))
  }
  else {
    
  }
  #Returning the multiple error values to the main function
  return(errvals)
}

Xgboost_simple_cv <- function(DMtrain,X_test,y_test,nround){
  #XGBoost using the simple cross validation on "nrounds"
  model <- xgboost(DMtrain, max_depth=2, eta=1, nthread=2, nrounds=nround, verbose=0, objective ="binary:logistic")
  pred <- predict(model, X_test)
  err <- mean(as.numeric(pred>0.5) != y_test)
  #Returning each err values back to model_dev function, which is getting saved inside "errvalues" list
  return(err)
}

#MAIN FUNCTION
simple_dict <- list()
for (x in 10^(2:7)){
  Rprof("simple_xgboost")
  errvals <- min(model_dev(data_generator(x)))
  Rprof(NULL)
  simple_dict[[as.character(x)]] <- list(
    error = errvals,
    profiling_time = summaryRprof("simple_xgboost")$sampling.time)
}

simple_xboost_err <- data.frame(
  x = as.numeric(names(simple_dict)),  
  error = sapply(simple_dict, function(x) x$error),  
  profiling_time = sapply(simple_dict, function(x) x$profiling_time) 
)

print(simple_xboost_err)
