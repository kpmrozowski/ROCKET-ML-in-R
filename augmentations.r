library(forecast)

reference_row_length <- function(data, fun) {
   # len <- Inf
   # for (i in seq_len(nrow(data))) {
   #    row <- data[i, seq_len(ncol(data))]
   #    len_i <- length(row[row != 0])
   #    if (len_i < len) {
   #       len <- len_i
   #    }
   # }

   len <- fun(
      apply(data, MARGIN = 1, FUN = function(row) {
         fun(length(row[row != 0]))
      })
   )
   return(len)
}

trim_data <- function(data, reference_length) {
  data <- t(apply(data, MARGIN = 1, FUN = function(row) {
      return(row[1:reference_length])
    }))
   return(data)
}

padding <- function(data) {
   reference_length <- reference_row_length(data, max)
   data <- trim_data(data, reference_length)
   data <- padding_inner(data)

   return(data)
}

padding_inner <- function(data) {
   for (i in seq_len(nrow(data))) {
      row <- data[i, seq_len(ncol(data))]
      data[i, row[row == 0]] <- -1
   }
   return(data)
}

cropping <- function(data) {
   reference_length <- reference_row_length(data, min)
   data <- trim_data(data, reference_length)

   return(data)
}

meanify <- function(data) {
   reference_length <- reference_row_length(data, min)
   data <- meanify_internal(data, reference_length)

   return(data)
}

meanify_internal <- function(data, reference_length) {
   new <- matrix(0, nrow(data), reference_length)
   for (i in seq_len(nrow(data))) {
      row <- data[i, seq_len(ncol(data))]
      row <- row[row != 0]
      dst <- abs(row - mean(row))
      top <- row[order(dst)][1:reference_length]
      idx <- unique(
         unlist(
            sapply(
               top,
               function(x) {
                  which(row == x)
               }
            )
         )
      )[1:reference_length]
      new[i, ] <- row[idx]
   }
   return(new)
}

extremify <- function(data) {
   reference_length <- reference_row_length(data, min)
   data <- extremify_internal(data, reference_length)

   return(data)
}

extremify_internal <- function(data, reference_length) {
   new <- matrix(0, nrow(data), reference_length)
   for (i in seq_len(nrow(data))) {
      row <- data[i, seq_len(ncol(data))]
      row <- row[row != 0]
      dst <- abs(row - mean(row))
      top <- row[order(dst, decreasing = T)][1:reference_length]
      idx <- unique(
         unlist(
            sapply(
               top,
               function(x) { which(row == x) }
            )
         )
      )[1:reference_length]
      new[i, ] <- row[idx]
   }
   return(new)
}

complement <- function(data) {
   reference_length <- reference_row_length(data, max)
   data <- trim_data(data, reference_length)
   data <- complement_internal(data, reference_length)

   return(data)
}

complement_internal <- function(data, reference_length) {
   new <- matrix(0, nrow(data), reference_length)
   for (i in seq_len(nrow(data))) {
      row <- data[i, seq_len(ncol(data))]
      row <- row[row != 0]
      row <- c(array(row, dim = c(1, reference_length)))
      new[i, ] <- row
   }
   return(new)
}

forecaster <- function(data) {
   reference_length <- reference_row_length(data, max)
   data <- trim_data(data, reference_length)
   data <- forecaster_internal(data, reference_length)

   return(data)
}

forecaster_internal <- function(data, reference_length) {
   return(
      as.matrix(
         do.call(
               rbind,
               mclapply(
                  seq_len(nrow(data)),
                  function(i) {
   row <- data[i, seq_len(ncol(data))]
   row <- row[row != 0]
   diff <- abs(reference_length - length(row))
   if (diff > 0) {
      a <- auto.arima(row)
      complement <- forecast(a, h = diff)
      row <- c(row, complement$mean)
   }
   return(row)
                  },
                  mc.cores = detectCores()
               )
         ),
         ncol = ncol(data)
      )
   )
}