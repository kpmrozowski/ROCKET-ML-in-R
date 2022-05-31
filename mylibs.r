
dat <- read.delim(file = "./data/AllGestureWiimoteX/AllGestureWiimoteX_TRAIN.txt", sep = "\t", header = FALSE) # nolint
df <- separate(dat,
               1,
               into = sprintf("x%s", 0:501),
               sep = " +",
               remove = TRUE,
               convert = TRUE,
               extra = "warn",
               fill = "warn"
               )[1:300, 2:501]


# print(x_out[a1 : b1, seq_len(ncol(x_out))])
# print(dim(x_out[a1 : b1, seq_len(ncol(x_out))]))
# print(length(applied_kernels))
