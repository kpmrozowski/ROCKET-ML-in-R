
library(tidyr) # separate
library(splitstackshape) # cSplit
library(imputeTS) # na_replace

####### SETTINGS
log_info <- TRUE
log_debug <- FALSE

dataset_names <- c(
   "./data/AllGestureWiimoteX/AllGestureWiimoteX",
   "./data/AllGestureWiimoteY/AllGestureWiimoteY",
   "./data/AllGestureWiimoteZ/AllGestureWiimoteZ",
   "./data/CricketX/CricketX",
   "./data/CricketY/CricketY",
   "./data/CricketZ/CricketZ",
   "./data/GestureMidAirD1/GestureMidAirD1",
   "./data/GestureMidAirD2/GestureMidAirD2",
   "./data/GestureMidAirD3/GestureMidAirD3",
   "./data/UWaveGestureLibraryX/UWaveGestureLibraryX",
   "./data/UWaveGestureLibraryY/UWaveGestureLibraryY",
   "./data/UWaveGestureLibraryZ/UWaveGestureLibraryZ"
)
num_runs <- 10
num_kernels <- 10000


results_all <- data.frame(
   datasets <- dataset_names,
   accuracy_mean <- replicate(length(dataset_names), 0),
   accuracy_standard_deviation <- replicate(length(dataset_names), 0),
   time_training_seconds <- replicate(length(dataset_names), 0),
   time_test_seconds <- replicate(length(dataset_names), 0)
)

cat("======= RUNNING RUNNING RUNNING =======\n")

for (dataset_name in dataset_names) {
   cat("Dataset:\t", dataset_name, "\n")

   ####### DATA LOADING
   cat("==== ======= data loading ======= =======\n")
   data_train <- as.data.frame(read.delim(file = paste(dataset_name, "_TRAIN.txt", sep=""), sep = "\t", header = FALSE)) # nolint
   data_test <- as.data.frame(read.delim(file = paste(dataset_name, "_TEST.txt", sep=""), sep = "\t", header = FALSE)) # nolint

   dat_train <- cSplit(data_train, 1, " +")
   dat_test <- cSplit(data_test, 1, " +")

   df_nan_train <- separate(
      data_train,
      1,
      into = sprintf("x%s", 0:ncol(dat_train)),
      sep = " +",
      remove = TRUE,
      convert = TRUE,
      extra = "warn",
      fill = "warn"
      )[seq_len(nrow(dat_train)), 2 : ncol(dat_train)]
   df_nan_test <- separate(data_test,
      1,
      into = sprintf("x%s", 0:ncol(dat_test)),
      sep = " +",
      remove = TRUE,
      convert = TRUE,
      extra = "warn",
      fill = "warn"
      )[seq_len(nrow(dat_test)), 2 : ncol(dat_test)]

   df_train <- na_replace(df_nan_train, 0)
   df_test <- na_replace(df_nan_test, 0)

   x_train <- df_train[2 : ncol(df_train)]
   x_test <- df_test[2 : ncol(df_test)]
   y_train <- df_train[1]
   y_test <- df_test[1]
   if (log_debug) {
      print(x_train[1 : 5, 1 : 10])
      print(x_train[1 : 5, (ncol(x_train) - 10) : ncol(x_train)])
      print(y_train[1:10, 1])
      print(y_train[(nrow(y_train) - 10) : nrow(y_train), 1])
      print(x_test[1 : 5, 1 : 10])
      print(x_test[1 : 5, (ncol(x_test) - 10) : ncol(x_test)])
      print(y_test[1:10, 1])
      print(y_test[(nrow(y_test) - 10) : nrow(y_test), 1])
   }
   cat("\tTrain dim:\t", dim(df_train), "\n")
   cat("\tTest dim:\t", dim(df_test), "\n")
   # print(dim(df_train))
   # print(dim(df_test))
   if (log_info) {
      cat("\tx_train dim:\t", dim(x_train), "\n")
      cat("\ty_train dim:\t", dim(y_train), "\n")
      cat("\tx_test dim:\t", dim(x_test), "\n")
      cat("\ty_test dim:\t", dim(y_test), "\n")
   }
   cat("==== ======= loading done ======= =======\n")

   ####### PERFORMING RUNS
   cat("==== ======= Performing runs ======= =======\n")
   results <- replicate(num_runs, 0)
   timings <- matrix(
      data = replicate(num_runs * 4, 0), ncol = 4, nrow = num_runs)

   # for (i in 1:num_runs) {
   #    input_length <- dim(x_train)[-1]
      
   # }
}