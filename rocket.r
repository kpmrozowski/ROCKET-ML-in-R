source("rocket_functions.r")
source("augmentations.r")

####### SETTINGS
log_info <- TRUE
log_debug <- FALSE

# augmentation <- cropping # 0.1
# augmentation <- padding # 0.1
# augmentation <- meanify # 0.6
# augmentation <- extremify # 0.1
# augmentation <- complement # 0.6
augmentation <- forecaster # 0.55

set.seed(123)
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
num_kernels <- 50

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

   cat("==== ======= data loading ======= =======\n")
   train <- load_df("TRAIN", dataset_name)
   test <- load_df("TEST", dataset_name)
   cat("\ttrain dim:\t", dim(train), "\n")
   cat("\ttest dim:\t", dim(test), "\n")

   # cat("==== removing in 90% empty rows =======\n")
   # train <- remove_almost_empty_rows(train)
   # test  <- remove_almost_empty_rows(test)
   # cat("\ttrai dim:\t", dim(train), "\n")
   # cat("\ttest dim:\t", dim(test), "\n")

   cat("==== ======= xy split ======= =======\n")
   train <- xy_split(train, "train")
   test  <- xy_split(test, "test")
   cat("\ttrain$x dim:\t", dim(train$x), "\n")
   cat("\ttest$x dim:\t", dim(test$x), "\n")

   cat("==== ======= augument data ======= =======\n")
   train$x <- augment_x(train$x, augmentation)
   test$x  <- augment_x(test$x,  augmentation)
   cat("\ttrain$x dim:\t", dim(train$x), "\n")
   cat("\ttest$x dim:\t", dim(test$x), "\n")

   cat("==== ======= Performing runs ======= =======\n")
   results <- replicate(num_runs, 0)
   timings <- matrix(
      data = replicate(num_runs * 4, 0), ncol = 4, nrow = num_runs)

   for (i in 1:num_runs) {
      input_length <- dim(train$x)[-1]
      kernels <- generate_kernels(dim(train$x)[-1], num_kernels)

      cat("==== ======= Transform training ======= =======\n")
      x_train_transform <- apply_kernels(train$x, kernels)
      cat("max: ", max(x_train_transform), "\n")
      cat("min: ", min(x_train_transform), "\n")

      cat("==== ======= Transform test ======= =======\n")
      x_test_transform <- apply_kernels(test$x, kernels)
      cat("max: ", max(x_test_transform), "\n")
      cat("min: ", min(x_test_transform), "\n")

      cat("==== ======= Choosing best alpha ======= =======\n")
      alpha <- choose_best_alpha(x_train_transform,
                                 train$y,
                                 x_test_transform,
                                 test$y)
      cat(alpha$best, "\n")
      cat(alpha$alphas, "\n")
      cat(alpha$accuracies, "\n")
   }

}