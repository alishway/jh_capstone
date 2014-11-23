source('~/GitHub/jh_capstone/JHCap_lib.R')

ptm <- proc.time()

#load("rawdata/002/tf.RData")

print(proc.time()-ptm)

cutoff <- .9
cf1 <- calcFreqCutoff(tf1, cutoff)

c.tf1 <- tf1[1:cf1]
#c.tf1 <- head(tf1, 10)

termStartEnd <- function (x) {
  return(c(paste0("^", x, " "), paste0(" ", x, "$")))
}

#c.tf2 <- tf2[1:10000]
c.tf2 <- tf2

chkBigram <- function (x, unigram) {
# this returns TRUE if all words in bigram x do NOT exist in unigram list 
  tr <- unlist(strsplit(x, " "))
  return((sum(tr %in% unigram) == 0))
}

print(proc.time()-ptm)

tr <- sapply(names(c.tf2), chkBigram, names(c.tf1))

print(proc.time()-ptm)

c.c.tf2 <- c.tf2[!tr]


#c.tf2 <- sort(tf2[tf2.idx], decreasing=TRUE)