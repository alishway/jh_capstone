require(Matrix)

#load("probidxC_700.RData")

lambda.set <- c(.1, .3, .6)


####
# convenience function to unlist integers while retaining length
# leaving empty cells as 0
####
deList <- function (n) {
  return(unlist(replace(n, !sapply(n, length), 0)))
#  return(sapply(n, function(x) {ifelse(length(x)==0, 0, x)}))
}


####
# reduce phrase into three words, padding 0 in front if fewer than 3
####
reduce <- function(phrase){
  words <- unlist(strsplit(phrase, " "))
  triplet <- character(3)
  len <- length(words)
  if (len > 0){
    st <- ifelse(len>2, len-2, 1)
    ti <- ifelse(len==1, 3, ifelse(len==2, 2, 1))
    triplet[ti:3] <- words[st:len]    
  }
  return(triplet)
}

####
# find index number of a word in vocab
# Assumption: vocab is loaded global all the time
####
index <- function(word) {
  if (word=="") return(0)
  id <- which(vocab == word)
  if (length(id) == 0) id <- 0
  return(id)
}

#####
# break phrases into triplet of indexes of last 3 words
# Assumption: vocab is loaded global all the time
#####
decompose <- function(phrase) {
  phrase <- tolower(phrase)
  phrase <- gsub("[[:punct:]]", "", phrase)
  words <- reduce(phrase)
  return(sapply(as.list(words), index))
}

####
# check non-zero next n-gram options
# inp is a triplet of the words as index of vocab (0 if not in vocab),
# and n to designate n-gram model to use (e.g. n=1 is unigram model)
# Assumption:
# - probability tables e.g.(pr.uni, pr.bi, and pr.tri are globally loaded)
# - vocab is globally loaded
# - input is a triplet with correct index (no error checking)
####
listNext <- function(inp) {
  uni.idx <- which(pr.uni$terms == inp[3])
  pos.idx.uni <- pr.uni$terms[which(pr.uni$prob[uni.idx,] != 0)]
  
  bi.idx <- which((pr.bi$terms[, 1] == inp[2]) & (pr.bi$terms[, 2] == inp[3]))
  pos.idx.bi <- pr.bi$terms[which(pr.bi$prob[bi.idx,] != 0), 2]

  tri.idx <- which((pr.tri$terms[, 1] == inp[1]) & (pr.tri$terms[, 2] == inp[2])
                   & (pr.tri$terms[, 3] == inp[3]))
  pos.idx.tri <- pr.tri$terms[which(pr.tri$prob[tri.idx,] != 0), 3]
  
  return(unique(c(pos.idx.uni, pos.idx.bi, pos.idx.tri)))

}

####
# calculate probability of a triple
# Assumption:
# - probability tables pr.uni, pr.bi, and pr.tri are loaded globally
# - universal lambda set is loaded globally
####
probTrip <- function(phrase) {
  words <- decompose(phrase)
  preds <- listNext(words)
  if (length(preds)==0) return(data.frame(idx=25072))   #if no prediction, default to "the"
  len <- length(preds)
  pred <- data.frame(idx=preds, # word=vocab[preds],
                     uni=numeric(len), bi=numeric(len), tri=numeric(len))
  
  #unigram probabilities
  uni.c <- deList(sapply(preds, function(x, y) {which(y == x)}, pr.uni$terms))
  pred$uni[uni.c != 0] <- pr.uni$prob[which(pr.uni$terms == words[3]), uni.c]
                                 

  bi.r <- which((pr.bi$terms[, 1] == words[2]) &
                    (pr.bi$terms[, 2] == words[3]))
  if (length(bi.r)) {  #do nothing if no preceding term
    bi.c <- deList(
      sapply(preds, function(x, y, z){which((y[, 1] == z[3]) & (y[, 2] == x))},
             pr.bi$terms, words)
    )
    pred$bi[bi.c!=0] <- pr.bi$prob[bi.r, bi.c]
  }

  

  tri.r <- which((pr.tri$terms[, 1] == words[1]) &
                 (pr.tri$terms[, 2] == words[2]) &
                 (pr.tri$terms[, 3] == words[3]))
  if (length(tri.r)) {  #do nothing if no preceding term
    tri.c <- deList(
      sapply(preds, function(x, y, z){
        which((y[, 1] == z[2]) & y[, 2] == z[3] & (y[, 3] == x))},
        pr.tri$terms, words)
    )
    pred$tri[tri.c!=0] <- pr.tri$prob[tri.r, tri.c]
  }


  pred$l.prob <- colSums(lambda.set*t(pred[, c("uni", "bi", "tri")]))

  return(pred[order(pred$l.prob, decreasing=TRUE),])
}

#####
# convenience function for predicting a word
#####
predWord <- function(phrase) {
  vocab[probTrip(phrase)$idx[1]]
}