source("assignment-data-generator.R")
library("xgboost")
library("purrr")
library("caret")

model_dev <- function(df){
  set.seed(100)
  train_index <- caret::createDataPartition(df$outcome, p=0.8, list=FALSE)
  train_data <- df[train_index,]
  test_data <- df[-train_index,]
  
  # Setting up 5-fold cross-validation
  fitControl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE
  )
  
  # Training using caret's train() and xgbTree method
  xgbfit <- train(outcome ~ .,  
                  data = train_data,
                  method = "xgbTree",
                  trControl = fitControl,
                  verbose = FALSE,
                  verbosity = 0)
  
  # Predict on the test set
  predvals <- predict(xgbfit, newdata = test_data)
  
  # Calculate error rate
  err <- mean(predvals != test_data$outcome)
  
  return(err)
}

# MAIN FUNCTION
cv_caret_dict <- list()

for (x in 10^(2:5)){
  Rprof("simple_xgboost")
  errval <- model_dev(data_generator(x))
  Rprof(NULL)
  cv_caret_dict[[as.character(x)]] <- list(
    error = errval,
    profiling_time = summaryRprof("simple_xgboost")$sampling.time
  )
}

cv_caret_xboost_err <- data.frame(
  x = as.numeric(names(cv_caret_dict)),
  error = sapply(cv_caret_dict, function(x) x$error),
  profiling_time = sapply(cv_caret_dict, function(x) x$profiling_time)
)

print(cv_caret_xboost_err)