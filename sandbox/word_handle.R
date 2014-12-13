vocab <- c("i", "love", "you", "we", "me", "read", "book", "movie")

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
# calculate probability of a tuple
# Assumption: probability tables prob.uni, prob.bi, and prob.tri are loaded
####


load("training/070/probC_700.RData")
vocab <- unique(c(prob.uni$terms, unlist(strsplit(prob.bi$terms, " ")),
                  unlist(strsplit(prob.tri$terms, " "))))
