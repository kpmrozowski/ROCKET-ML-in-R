source("rocket_functions_multivariate.r")

####### SETTINGS
num_runs <- 10
num_kernels <- 200
replace_with_mean <- T
seed0 <- 123

results_path <- "results_1"
datasets_path <- "data"
datasets <- read_datases_names_and_counts(datasets_path)
print(datasets$names)
print(datasets$counts)

dir.create(paste0(getwd(), "/", results_path), showWarnings = FALSE)
results <- data.frame(
   filename <- datasets$names,
   total_seconds <- numeric(length(datasets$names)),
   preprocessing_seconds <- numeric(length(datasets$names)),
   rocket_seconds <- numeric(length(datasets$names)),
   evaluation_seconds <- numeric(length(datasets$names)),
   accuracy_max <- numeric(length(datasets$names)),
   accuracy_min <- numeric(length(datasets$names)),
   accuracy_mean <- numeric(length(datasets$names)),
   accuracy_std <- numeric(length(datasets$names)),
   accuracy_median <- numeric(length(datasets$names)),
   alpha_max <- numeric(length(datasets$names)),
   alpha_min <- numeric(length(datasets$names)),
   alpha_mean <- numeric(length(datasets$names)),
   alpha_std <- numeric(length(datasets$names)),
   alpha_median <- numeric(length(datasets$names))
)

cat("\n\n======= RUNNING RUNNING RUNNING =======\n")

for (i in seq_len(length(datasets$names))) {
   start <- Sys.time()
   dataset_name <- datasets$names[i]
   dataset_dims <- datasets$counts[i]

   cat("\tDimensions:  ", dataset_dims,
       "\tDataset:   ", dataset_name, "\n")

   total_time <- 0
   preprocessing_time <- 0
   rocket_time <- 0
   evaluation_time <- 0

   cat("======= ======= Data loading ======= =======\n")
   train <- read_data("TRAIN", datasets_path, dataset_name, dataset_dims)
   test <- read_data("TEST", datasets_path, dataset_name, dataset_dims)
   cat("\ttrain dim:\t", dim(train$data_sets), "\n")
   cat("\ttest dim:\t", dim(test$data_sets), "\n")
   dir.create(
      paste0(getwd(), "/", results_path, "/", dataset_name),
      showWarnings = FALSE)
   write.table(train$classes, row.names = F, sep = ",", file = paste0(
      getwd(), "/", results_path, "/", dataset_name, "/Y_TRAIN.txt"))
   write.table(test$classes, row.names = F, sep = ",", file = paste0(
      getwd(), "/", results_path, "/", dataset_name, "/Y_TEST.txt"))

   max_accuaracies <- numeric(length(num_runs))
   best_alphas <- numeric(length(num_runs))
   for (run_id in seq_len(num_runs)) {
      cat("\tIteration", run_id, "of", num_runs, "\n")

      cat("======= ======= Replacing NaN ======= =======\n")
      preprocessing_start <- Sys.time()
      replaced_trainset <- replace_values(
         train$data_sets, train$classes, replace_with_mean, seed0 + run_id)
      write.table(replaced_trainset, row.names = F, sep = ",", file = paste0(
         getwd(), "/", results_path, "/", dataset_name, "/replaced_TRAIN.txt"))

      replaced_testset <- replace_values(
         test$data_sets, test$classes, replace_with_mean, seed0 + run_id)
      write.table(replaced_testset, row.names = F, sep = ",", file = paste0(
         getwd(), "/", results_path, "/", dataset_name, "/replaced_TEST.txt"))

      preprocessing_time <- preprocessing_time +
         as.numeric(Sys.time() - preprocessing_start, units = "secs")

      cat("======= ======= Generating kernels ======= =======\n")
      rocket_start <- Sys.time()
      kernels <- generate_kernels(
         dim(train$data_sets)[3], num_kernels, dataset_dims, seed0 + run_id)

      cat("======= ======= Rocketing trainset ======= =======\n")
      train_transform <- apply_kernels(replaced_trainset, kernels)
      write.table(train_transform, row.names = F, sep = ",", file = paste0(
         getwd(), "/", results_path, "/", dataset_name, "/Rocketed_TRAIN.txt"))

      cat("======= ======= Rocketing testset ======= =======\n")
      test_transform <- apply_kernels(replaced_testset, kernels)
      write.table(test_transform, row.names = F, sep = ",", file = paste0(
         getwd(), "/", results_path, "/", dataset_name, "/Rocketed_TEST.txt"))

      rocket_time <- rocket_time +
         as.numeric(Sys.time() - rocket_start, units = "secs")

      cat("======= ======= Evaluation ======= =======\n")
      evaluation_start <- Sys.time()
      evaluation <- evaluate_model(train_transform,
                                train$classes,
                                test_transform,
                                test$classes)
      evaluation_time <- evaluation_time +
         as.numeric(Sys.time() - evaluation_start, units = "secs")

      cat("\taccuracy:", evaluation$best_accuracy,
         "\talpha:", evaluation$best_alpha, "\n")
      max_accuaracies[run_id] <- evaluation$best_accuracy
      best_alphas[run_id] <- evaluation$best_alpha
   }
   results$filename[i] <- datasets$names[i]
   results$total_seconds[i] <- as.numeric(Sys.time() - start, units = "secs")
   results$preprocessing_seconds[i] <- preprocessing_time
   results$rocket_seconds[i] <- rocket_time
   results$evaluation_seconds[i] <- evaluation_time
   results$accuracy_max[i] <- max(max_accuaracies)
   results$accuracy_min[i] <- min(max_accuaracies)
   results$accuracy_mean[i] <- mean(max_accuaracies)
   results$accuracy_std[i] <- sd(max_accuaracies)
   results$accuracy_median[i] <- median(max_accuaracies)
   results$alpha_max[i] <- max(best_alphas)
   results$alpha_min[i] <- min(best_alphas)
   results$alpha_mean[i] <- mean(best_alphas)
   results$alpha_std[i] <- sd(best_alphas)
   results$alpha_median[i] <- median(best_alphas)

   cat("#### Rocket time was:", results$total_seconds[i], "seconds ####\n")
   write.csv(
      results,
      file = paste0(
         getwd(), "/", results_path, "/RESULTS.txt")
   )
}
