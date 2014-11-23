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
cf2 <- calcFreqCutoff(tf2, cutoff)
c.tf2 <- tf2[1:cf2]
#c.tf2 <- head(tf2, 10)
#c.tf2 <- tf2

#c.tf3 <- tf3[1:1000]
c.tf3 <- tf3

chkBigram <- function (x, unigram) {
# this returns TRUE if all words in bigram x do NOT exist in unigram list 
  tr <- unlist(strsplit(x, " "))
  return(sum(tr %in% unigram) == 0)
}

chkTrigram <- function (x, bigram) {
# this returns TRUE if all word couple in trigram x do NOT exist in bigram list 
  words <- unlist(strsplit(x, " "))
  pairs <- c(paste(words[1], words[2]), paste(words[2], words[3]))
  return(sum(pairs %in% bigram) == 0)
}

print(proc.time()-ptm)

#tr <- sapply(names(c.tf2), chkBigram, names(c.tf1))
tr <- sapply(names(c.tf3), chkTrigram, names(c.tf2))

print(proc.time()-ptm)

#c.c.tf2 <- c.tf2[!tr]
c.c.tf3 <- c.tf3[!tr]

#c.tf2 <- sort(tf2[tf2.idx], decreasing=TRUE)