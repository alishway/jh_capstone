### library of prediction functions for JH Capstone project on NLP

source('~/GitHub/jh_capstone/JHCap_lib.R')

####
# convenience function to load prestore term frequencies based on coverage
# coverage is from 0 to 1
####
loadTF <- function (coverage) {
  load(sprintf("dictionaries/tf_%03.0f.RData", coverage*1000), .GlobalEnv)
}

####
# count bigram probability of term following pre, with
# tf1 unigram term frequency for pre and tf2 bigram term frequency for pre+term
####
prob1 <- function (term, pre, tf1, tf2) {
  bigram <- paste(pre, term)
  c.bigram <- tf2[bigram]
  c.pre <- tf1[pre]
  prob <- c.bigram/c.pre
  #Assign 0 to non-numeric result
  prob[!is.finite(prob)] <- 0
  return(prob)  
}

logProbPhrase <- function (phrase, tf1, tf2) {
  #tokenize to clean
  
  words <- unlist(strsplit(tolower(phrase), " "))
  pairs <- 2:length(words)
  log.probs <- sapply(pairs, function(x) {
    return(log(prob1(words[x], words[x-1], tf1, tf2)))})
  return(sum(log.probs))
}

probPhrase <- function (phrase, tf1, tf2) {
  lp <- logProbPhrase(phrase, tf1, tf2)
  return(exp(lp))
}

predictList <- function (pre, tf1, tf2, fun=prob1) {
  candidates <- names(tf1)
  return(fun(candidates, pre, tf1, tf2))
}

predictListCount <- function (pre, tf1, tf2) {
  candidates <- paste(pre, names(tf1))
  long.list <- sort(tf2[candidates], decreasing=TRUE)
  return(long.list[!is.na(long.list)])
}


####
# predict next word after phrase with simple backoff:
# check trigram first, then bigram if not found
# ignore unigram; it's always "the" at the top
####
predSimpleBackoff <- function (phrase, tf1, tf2, tf3) {
  corp <- VCorpus(VectorSource(phrase))
  corp <- cleanCorp(corp)
  words <- unlist(strsplit(corp[[1]]$content, " "))
  len <- length(words)
  if (len > 1) {
    bigram <- paste(words[len-1], words[len])
    pred <- predictListCount(bigram, tf1, tf3)[1]
    if (is.na(pred)) {
      pred <- predictListCount(words[len], tf1, tf2)[1]
    }

  } else if (len == 1) {
    pred <- predictListCount(words, tf1, tf2)[1]
  } else pred <- NA
  
  return(pred)
}

####
# predict next word after phrase with simple backoff using quadgram:
# check quadgram first, then trigram if not found, then bigram
# ignore unigram; it's always "the" at the top
####
predSimpleBackoffQuad <- function (phrase, tf1, tf2, tf3, tf4) {
  len <- length(unlist(strsplit(phrase, " ")))
  if (len > 2) {
    corp <- VCorpus(VectorSource(phrase))
    corp <- cleanCorp(corp)
    words <- unlist(strsplit(corp[[1]]$content, " "))
    trigram <- paste(words[len-2], words[len-1], words[len])
    pred <- predictListCount(trigram, tf1, tf4)[1]
    if (is.na(pred)) {
      pred <- predSimpleBackoff(paste(words[len-1], words[len]),
                                tf1, tf2, tf3)[1]
    }
    
  } else {
    pred <- predSimpleBackoff(phrase, tf1, tf2, tf3)
  }  
  
  return(pred)
}

####
# predict with using ngram model
# wordlist is sequence of word of phrase to be predicted,
# after cleanup and separating into individual words
####
predNgram <- function (wordlist, n, model) {
  # TO DO: error check to prevent n being incompatible with tf
  # (i.e. n=1 must be unigram model tf)
  len <- length(wordlist)
  preterm <- paste(wordlist[(len-n+1):len], collapse=" ")
  maxrow <- which(model$terms == preterm)
  if (length(maxrow) == 0) pred <- NA
  else {
    maxcnt <- max(model$counts[maxrow,])
    if (maxcnt == 0) {  # i.e. preterm not found in n-gram
      #just eject. backoff to lower Ngram to be handled in wrapper function
      pred <- NA
    } else {
      maxcol <- which(model$counts[maxrow, ] == maxcnt)
      pred <- model$terms[maxcol[1]]  # subscript 1 in case of ties      
    }
  }
  return(pred)
}

compPredNgram <- cmpfun(predNgram)

####
# predict with simple backoff using ngram model:
# check trigram model first (built from quadgram list)
# then bigram if not found and lastly unigram model 
####
predNgramBackoff <- function (phrase, unimod, bimod, trimod) {
  wordlist <- unlist(strsplit(compPrepText(phrase), " "))
  pred <- NA
  len <- length(wordlist)
  if (len > 3) {
    pred <- predNgram(wordlist, 3, trimod)
  }
  if (is.na(pred) & (len > 2)) {
    pred <- predNgram(wordlist, 2, bimod)
  }
  if (is.na(pred)) {
    pred <- predNgram(wordlist, 1, unimod)
  }
  return(pred)
}