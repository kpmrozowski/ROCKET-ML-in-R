library(tidyr) # separate
library(splitstackshape) # cSplit
library(imputeTS) # na_replace


load_df <- function(name, path) {

   data <- as.data.frame(
      read.delim(
         file = paste(dataset_name, "_", name, ".txt", sep = ""),
         sep = "\t", header = FALSE)
   )

   dat <- cSplit(data, 1, " +")

   df_nan <- separate(
      data,
      1,
      into = sprintf("x%s", 0:ncol(dat)),
      sep = " +",
      remove = TRUE,
      convert = TRUE,
      extra = "warn",
      fill = "warn"
      )[seq_len(nrow(dat)), 2 : ncol(dat)]

   return(na_replace(df_nan, 0)) # nolint
}

remove_almost_empty_rows <- function(data) {
   removed_count <- 0
   for (i in seq_len(nrow(data))) {
      row <- data[i, ]
      if (length(row[row == 0]) > (length(row) * 0.9)) {
         data <- data[- (i - removed_count), ]
         removed_count <- removed_count + 1
      }
   }
   return(data)
}

xy_split <- function(df, name) {
   x <- df[2 : ncol(df)]
   y <- df[1]
   return(list(
      "x" = x,
      "y" = y
   ))
}

augment_x <- function(data, augment) {
   return(augment(data))
}

generate_kernels <- function(input_len, num_kernels) {
   candidate_lengths <- c(7, 9, 11)
   lengths <- sample(candidate_lengths, size = num_kernels, replace = T)

   weights_all <- replicate(sum(lengths), 0)
   biases      <- replicate(num_kernels, 0)
   dilations   <- replicate(num_kernels, 0)
   paddings    <- replicate(num_kernels, 0)

   a1 <- 1
   for (i in 1:num_kernels) {
      len <- lengths[i]
      weights <- rnorm(len)

      b1 <- a1 + len - 1
      weights_all[a1 : b1] <- weights - mean(weights)
      biases[i] <- runif(1)

      nlog2 <- log2((input_len - 1) / (len - 1))
      nrunif <- runif(1, min = 0, max = nlog2)
      dilation <- 2 ^ nrunif
      dilations[i] <- floor(dilation)

      if (sample(c(0, 1), 1) == 1) {
         padding <- ((len - 1) * dilation) %/% 2
      } else {
         padding <- 0
      }
      paddings[i] <- padding

      a1 <- b1
   }
   return(list(
      "weights" = weights_all,
      "lengths" = lengths,
      "biases" = biases,
      "dilations" = dilations,
      "paddings" = paddings
   ))
}

apply_kernel <- function(x_in, weights, len, bias, dilation, padding) {
   input_len <- length(x_in)
   output_len <- (input_len + (2 * padding)) - ((len - 1) * dilation)

   ppv <- 0
   max <- -Inf

   end <- (input_len + padding) - ((len - 1) * dilation)

   for (i in -padding : (end - 1)) {
      sum <- bias
      index <- i
      for (j in seq_len(len)) {
         if (index > 0 & index - 1 < input_len) {
               sum <- sum + weights[j] * x_in[index]
         }
         index <- index + dilation
      }
      if (sum > max) {
         max <- sum
      }
      if (sum > 0) {
         ppv <- ppv + 1
      }
   }
   ret1 <- ppv / output_len
   ret <- c(ret1, max)
   return(ret)
}

library(parallel)
library(lme4)

apply_kernels <- function(x_in, kernels) {

   weights <- kernels$weights
   lengths <- kernels$lengths
   biases <- kernels$biases
   dilations <- kernels$dilations
   paddings <- kernels$paddings

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
   a1 <- 1 # for weights
   for (j in seq_len(num_kernels)) {
      a_x_out_row[(2 * j - 1):(2 * j)] <- apply_kernel(
         x_in[i, seq_len(ncol(x_in))],
         weights[a1:(a1 + lengths[j] - 1)],
         lengths[j],
         biases[j],
         dilations[j],
         paddings[j]
      )
      a1 <- a1 + lengths[j]
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

choose_best_alpha <- function(train_data,
                              train_classes,
                              test_data,
                              test_classes) {
   alphas <- 1.1 ^ (-100:50)
   accuracies <- do.call(rbind, mclapply(alphas, FUN = function(alpha) {
      res <- evaluate_transform(
         train_data, train_classes, test_data, test_classes, alpha)
      return(res$accuracy)
   }))
   # accuracies <- sapply(alphas, FUN = function(alpha) {
   #    res <- evaluate_transform(
   #       train_data, train_classes, test_data, test_classes, alpha)
   #    return(res$accuracy)
   # })
   accuracies <- unlist(accuracies)
   idx <- which(accuracies == max(accuracies))[1]

   return(list(
         "best" = alphas[idx],
         "alphas" = alphas,
         "accuracies" = accuracies))
}
