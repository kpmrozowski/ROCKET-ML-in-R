
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
