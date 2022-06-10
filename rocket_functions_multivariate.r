library(tidyr) # separate
library(splitstackshape) # cSplit
library(stringr)

read_datases_names_and_counts <- function(datasets_path) {

   datasets_names <- list.dirs(datasets_path)[-1]
   counts <- numeric(length(datasets_names))
   i <- 1
   for (name in datasets_names) {
      dir_path <- name
      counts[i] <-
         length(
            list.files(
               path = dir_path,
               pattern = ".*Dimension.*.arff"
            )
         ) / 2
      i <- i + 1
   }
   for (i in seq_len(length(datasets_names))) {
      tmp <- unlist(str_split(datasets_names[i], "/"))
      datasets_names[i] <- tmp[length(tmp)]
   }
   return(list(
      "names" = datasets_names,
      "counts" = counts)
   )
}

library("foreign")

read_data <- function(train_or_test, datasets_path, name, count) {

   dataset_path <- paste0(
      getwd(), "/", datasets_path, "/", name, "/")
   data <- read.arff(paste0(
      dataset_path, name, "Dimension1_", train_or_test, ".arff"))
   classes <- data[, ncol(data)]
   data <- data[, 1:(ncol(data) - 1)]
   data_sets <- array(numeric(), c(nrow(data), count, ncol(data)))
   data_sets[, 1, ] <-
      array(data = c(unlist(data)), dim = c(nrow(data), ncol(data)))

   for (i in 2:count) {
      cat(i, " ")
      data <- read.arff(paste0(
         dataset_path, name, "Dimension", i, "_", train_or_test, ".arff"))
      data <- data[, 1:(ncol(data) - 1)]
      data_sets[, i, ] <-
         array(data = c(unlist(data)), dim = c(nrow(data), ncol(data)))
   }
   cat("\n")
   list("data_sets" = data_sets, "classes" = classes)
}

replace_values <- function(datasets, classes, replace_with_mean, seed) {

   set.seed(seed)
   classes_table <- table(classes)
   unique_classes <- unique(classes)
   dims <- dim(datasets)
   

   for (i in seq_len(length(unique_classes))) {
      one_third <- as.integer(classes_table[[unique_classes[i]]] / 3)
      two_third <- 2 * one_third
      counter <- 0
      for (j in seq_len(length(classes))) {
         if (classes[j] == unique_classes[i]) {
            counter <- counter + 1
            if (counter < one_third) {
               from <- sample(
                  as.integer(dims[3] * 0.1) : as.integer(dims[3] * 0.4), 1)
            }
            else if (counter >= one_third & counter < two_third) {
               from <- sample(
                  as.integer(dims[3] * 0.4) : as.integer(dims[3] * 0.7), 1)
            }
            else {
               from <- sample(as.integer(dims[3] * 0.7) : dims[3], 1)
            }
            for (dim_id in seq_len(dims[2])) {
               if (replace_with_mean) {
                  datasets[j, dim_id, from:dims[3]] <- rep(mean(
                     datasets[j, dim_id, 1:(from - 1)]), dims[3] - from + 1)
               }
               else {
                  datasets[j, dim_id, from:dims[3]] <-
                     numeric(dims[3] - from + 1)
               }
            }
         }
      }
   }
  return(datasets)
}

generate_kernels <- function(timepoints_count, num_kernels, ncols, seed) {

   set.seed(seed)
   candidate_lengths <- c(7, 9, 11)
   lengths <- sample(candidate_lengths, num_kernels, replace = TRUE)
   channel_lengths <- integer(num_kernels)

   for (i in seq_len(num_kernels)) {
      limit <- min(ncols, lengths[i])
      channel_lengths[i] <- as.integer(2 ** runif(1, 0, log2(limit + 1)))
   }
   channel_indices <- integer(sum(channel_lengths))
   weights_all <- integer(as.integer(lengths %*% channel_lengths))
   biases <- integer(num_kernels)
   dilations <- integer(num_kernels)
   paddings <- integer(num_kernels)

   a1 <- 1
   a2 <- 1
   for (i in seq_len(num_kernels)) {
      weights <- rnorm(channel_lengths[i] * lengths[i], 0, 1)

      a3 <- 1
      for (j in 1:channel_lengths[i]) {
         weights[a3:(a3 + lengths[i] - 1)] <- weights[a3:(a3 + lengths[i] - 1)]
            - mean(weights[a3:(a3 + lengths[i] - 1)])
         a3 <- a3 + lengths[i]
      }
      weights_all[a1:(a1 + channel_lengths[i] * lengths[i] - 1)] <- weights
      channel_indices[a2:(a2 + channel_lengths[i] - 1)] <-
         sample(seq(0, ncols - 1), channel_lengths[i])
      biases[i] <- runif(1, -1, 1)
      nlog2 <- log2((timepoints_count - 1) / (lengths[i] - 1))

      if (nlog2 > 0) {
         dilations[i] <- as.integer(2 ** runif(1, 0, nlog2))
      }
      else {
         dilations[i] <- as.integer(2 ** runif(1, nlog2, 0))
      }
      if (sample(c(0, 1), 1) == 0) {
         paddings[i] <- as.integer(((lengths[i] - 1) * dilations[i]) / 2)
      }
      else {
         paddings[i] <- 0
      }
      a1 <- a1 + channel_lengths[i] * lengths[i]
      a2 <- a2 + channel_lengths[i]
   }
   return(list(
      "weights" = weights_all,
      "lengths" = lengths,
      "biases" = biases,
      "dilations" = dilations,
      "paddings" = paddings,
      "channel_lengths" = channel_lengths,
      "channel_indices" = channel_indices)
   )
}

apply_kernel <- function(x_in, weights, len, bias, dilation, padding,
   channel_lengths, channel_indices) {

   timepoints_count <- dim(x_in)[2]
   end <- timepoints_count + padding - (len - 1) * dilation
   max1 <- -Inf
   ppv <- 0
   for (index in -padding:end - 1) {
      sum1 <- bias
      for (j in seq_len(len)) {
         if (index > -1 & index < timepoints_count) {
            for (k in seq_len(channel_lengths)) {
               sum1 <-
                  sum1 + weights[k, j] * x_in[channel_indices[k] + 1, index + 1]
            }
         }
         index <- index + dilation
      }
      if (sum1 > max1) {
         max1 <- sum1
      }
      if (sum1 > 0) {
         ppv <- ppv + 1
      }
   }
   output_length <- timepoints_count + 2 * padding - (len - 1) * dilation
   return(c(ppv / output_length, max1))
}

apply_kernel_uni <- function(x_in, weights, len, bias, dilation, padding) {

   timepoints_count <- length(x_in)
   ppv <- 0
   max_ <- -Inf
   for (i in -padding:(timepoints_count + padding - (len - 1) * dilation)) {
      sum_ <- bias
      index <- -padding
      for (j in seq_len(len)) {
         if (index > -1 & index < timepoints_count) {
            sum_ <- sum_ + weights[j] * x_in[index + 1]
         }
         index <- index + dilation
      }
      if (sum_ > max_) {
         max_ <- sum_
      }
      if (sum_ > 0) {
         ppv <- ppv + 1
      }
   }
   output_length <- timepoints_count + 2 * padding - (len - 1) * dilation
   return(c(ppv / output_length, max_))
}

library(parallel)

apply_kernels <- function(x_in, kernels) {

   weights <- kernels$weights
   lengths <- kernels$lengths
   biases <- kernels$biases
   dilations <- kernels$dilations
   paddings <- kernels$paddings
   channel_lengths <- kernels$channel_lengths
   channel_indices <- kernels$channel_indices
   num_examples <- dim(x_in)[1]
   num_kernels <- length(lengths)

   return(
      as.matrix(
         do.call(
               rbind,
               mclapply(
                  seq_len(num_examples),
                  function(i) {
   a_x_out_row <- replicate(num_kernels * 2, 0)
   a1 <- 1
   a2 <- 1
   a3 <- 1
   for (j in seq_len(num_kernels)) {
      if (channel_lengths[j] == 1) {
         a_x_out_row[c(a3:(a3 + 1))] <- apply_kernel_uni(
            x_in[i, channel_indices[a2] + 1, ],
            weights[a1:(a1 + channel_lengths[j] * lengths[j] - 1)],
            lengths[j],
            biases[j],
            dilations[j],
            paddings[j])
      }
      else {
         weights_reshaped <- array_reshape(
            weights[a1:(a1 + channel_lengths[j] * lengths[j] - 1)],
            c(channel_lengths[j], lengths[j]))

         a_x_out_row[c(a3:(a3 + 1))] <- apply_kernel(
            x_in[i, , ],
            weights_reshaped,
            lengths[j],
            biases[j],
            dilations[j],
            paddings[j],
            channel_lengths[j],
            channel_indices[a2:(a2 + channel_lengths[j] - 1)]
         )
      }
      a1 <- a1 + channel_lengths[j] * lengths[j]
      a2 <- a2 + channel_lengths[j]
      a3 <- a3 + 2
   }
   return(a_x_out_row)
                  },
                  mc.cores = detectCores()
               )
         ),
         ncol = num_kernels * 2
      )
   )
}

library(reticulate)
use_python("./env/bin/python3.10")

evaluate_transform <- function(train_data,
                               train_classes,
                               test_data,
                               test_classes,
                               alpha = array(c(0.1, 1.0, 10.0))) {

   sklearn_preprocessing <- import("sklearn.preprocessing")
   sklearn_pipeline <- import("sklearn.pipeline")
   sklearn_metrics <- import("sklearn.metrics")
   sklearn_model <- import("sklearn.linear_model")

   classifier <- sklearn_pipeline$make_pipeline(
      sklearn_preprocessing$StandardScaler(with_mean = F),
      sklearn_model$RidgeClassifierCV(alpha)
   )
   x_train_df <- data.frame(DATA = train_data)
   x_test_df  <- data.frame(DATA = test_data)
   train_classes <- as.vector(unname(as.matrix(train_classes)))
   test_classes <- as.vector(unname(as.matrix(test_classes)))
   classifier$fit(x_train_df, train_classes)

   predictions <- classifier$predict(x_test_df)
   accuracy <- sklearn_metrics$accuracy_score(predictions, test_classes)

   return(list(
      "accuracy" = accuracy,
      "predictions" = predictions))
}

evaluate_model <- function(train_data,
                              train_classes,
                              test_data,
                              test_classes) {
   alphas <- 1.5 ^ (-18:18)
   accuracies <- do.call(rbind, mclapply(alphas, FUN = function(alpha) {
      res <- evaluate_transform(
         train_data, train_classes, test_data, test_classes, alpha)
      return(res$accuracy)
   }))
   accuracies <- unlist(accuracies)
   idx <- which(accuracies == max(accuracies))[1]

   return(list(
         "best_alpha" = alphas[idx],
         "best_accuracy" = accuracies[idx],
         "alphas" = alphas,
         "accuracies" = accuracies))
}
