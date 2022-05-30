

addition <- function(a, b, c) {
    return (a + b + c)
}

generate_kernels <- function(input_length, num_kernels) {
   candidate_lengths <- c(7, 9, 11)
   lengths <- sample(candidate_lengths, size = num_kernels, replace = T)
}

