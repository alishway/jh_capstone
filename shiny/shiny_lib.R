require(Matrix)
#vocab <- c("i", "love", "you", "we", "me", "read", "book", "movie")
lambda.set <- c(.1, .3, .6)


####
# convenience function to unlist integers while retaining length
# leaving empty cells as NA
####
deList <- function (n) {
  return(sapply(n, `length<-`, 1))
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
  pos.idx.uni <- which(pr.uni$prob[uni.idx,] != 0)
  
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
  len <- length(preds)
  pred <- data.frame(word=preds,
                     uni=numeric(len), bi=numeric(len), tri=numeric(len))
  
  uni.f <- preds %in% pr.uni$terms
  pred$uni[uni.f] <- pr.uni$prob[which(pr.uni$terms == words[3]),
                                 which(pr.uni$terms %in% preds)]
  
  #  bi.f <- (pr.bi$terms[, 1] == words[3]) & (pr.bi$terms[, 2] %in% preds)
  bi.r <- which((pr.bi$terms[, 1] == words[2]) &
                  (pr.bi$terms[, 2] == words[3]))
  bi.c <- deList(sapply(pred$word,
                        function(x, y, z){which((y$terms[, 1] == z[3]) & (y$terms[, 2] == x))},
                        pr.bi, words))
  #  bi.c <- which((pr.bi$terms[, 1] == words[3]) &
  #                   (pr.bi$terms[, 2] %in% preds))
  pred$bi[!is.na(bi.c)] <- pr.bi$prob[bi.r, bi.c]
  
  #  tri.f <- (pr.tri$terms[, 1] == words[2]) & (pr.tri$terms[, 2] == words[3]) &
  #           (pr.tri$terms[, 3] %in% preds)
  tri.r <- which((pr.tri$terms[, 1] == words[1]) &
                   (pr.tri$terms[, 2] == words[2]) &
                   (pr.tri$terms[, 3] == words[3]))
  tri.c <- deList(sapply(pred$word,
                         function(x, y, z){which((y$terms[, 1] == z[3]) & (y$terms[, 2] == x))},
                         pr.tri, words))
  #  tri.c <- which((pr.tri$terms[, 1] == words[2]) &
  #                 (pr.tri$terms[, 2] == words[3]) &
  #                   (pr.tri$terms[, 3] %in% preds))
  pred$tri[!is.na(tri.c)] <- pr.tri$prob[tri.r, tri.c]
}