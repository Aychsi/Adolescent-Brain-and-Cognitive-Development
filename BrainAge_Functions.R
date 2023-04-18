
### File with Brain Age KDM Functions ###


## Split into and test set for healthy cohort ##
# Input: healthy cohort df. NB: Should include columns interview_age and sex
# Output: list of two dataframes called train_test (train and test)

healthy_split <- function (df) {
  
  # Rename necessary columns
  names(df) <- sub(".x$", "", names(df))
  names(df) <- sub(".y$", "", names(df))
  df$age <- as.numeric(df$interview_age)/12
  df$gender <- df$sex
  
  # Split
  set.seed(18)
  
  ## Healthy Training ##
  samp <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.6,0.4))
  train <- df[samp, ]
  test <- df[!samp, ]
  
  train <- train[complete.cases(train), ]
  test <- test[complete.cases(test), ]
  
  # List to store two df's
  train_test <- list()
  
  train_test$train <- train
  train_test$test <- test
  
  return(train_test)
  
}


## Train ##
# Input: train: training df from healthy_split function; bm_string: string of biomarker of interest
# Output: df called kdm_data_train which has calculated kdm and kdm_advance

kdm_train <- function (train, bm_string) {
  
  markers <- colnames(train %>% dplyr::select(contains(bm_string)))
  
  kdm_data_train <- kdm_calc(train, biomarkers = markers)
  
  return(kdm_data_train)
}


## Project to External DataFrame ##
# Input: test: test df from healthy_split function; kdm_data_train: kdm_data_train from kdm_train function; bm_string: string of biomarker of interest
# Output: df called kdm_data_test which has calculated with an external dataset with the parameters of the training set

kdm_test <- function(df, kdm_data_train, bm_string) {
  
  # Rename necessary columns
  df$age <- as.numeric(df$interview_age)/12
  df$gender <- df$sex
  
  markers <- colnames(kdm_data_train$data %>% dplyr::select(contains(bm_string)))
  
  kdm_test <- kdm_calc(df, biomarkers = markers, fit = kdm_data_train$fit, s_ba2 = 
                               kdm_data_train$fit$s_ba2)
  
  kdm_data_test <- kdm_test$data
  
  return(kdm_data_test)
  
}


## Get Validation Metrics ##
# Input: kdm_data_test: kdm_data_test from kdm_test function
# Output: prints correlation, rmse, mae, and plot of KDM vs CA

get_val_metrics <- function(kdm_data_test) {
  
  print(cor.test(as.numeric(kdm_data_test$kdm), as.numeric(kdm_data_test$age)))
  print(rmse(as.numeric(kdm_data_test$kdm), as.numeric(kdm_data_test$age)))
  print(mae(as.numeric(kdm_data_test$kdm), as.numeric(kdm_data_test$age)))
  
  #plot age vs bioage
  # print(plot_ba(kdm_data_test, "kdm", "KDM\nBiological Age"))
}




