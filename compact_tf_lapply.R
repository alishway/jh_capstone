# DEPRECATED: see compact_tf.R, much faster

source('~/GitHub/jh_capstone/JHCap_lib.R')

ptm <- proc.time()

#load("rawdata/002/tf.RData")

print(proc.time()-ptm)

cutoff <- .9
cf1 <- calcFreqCutoff(tf1, cutoff)

c.tf1 <- tf1[1:cf1]
#c.tf1 <- head(tf1, 10)
c.tf2 <- tf2[1:10000]

termStartEnd <- function (x) {
  return(c(paste0("^", x, " "), paste0(" ", x, "$")))
}


print(proc.time()-ptm)

tf2.idx <- sort(unique(unlist(
  lapply(termStartEnd(names(c.tf1)), grep, names(c.tf2))
)))

print(proc.time()-ptm)

c.c.tf2 <- sort(c.tf2[tf2.idx], decreasing=TRUE)

oc.c.tf2 <- c.c.tf2