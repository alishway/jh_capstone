### library of prediction functions for JH Capstone project on NLP

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
  long.list <- tf2[candidates]
  return(long.list[!is.na(long.list)])
}