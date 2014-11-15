### library of functions for JH Capstone project on NLP

require(tm)
require(RWeka)

####
# create a corpus from raw character afer removing 
####
CreateCorp <- function (chardata, tag_orig) {
  corp <- VCorpus(VectorSource(chardata))
  
  #tag source of sentence
  corp <- tm_map(corp, function(x) {
    meta(x, "origin") <- tag_orig
    x
  })
  return(corp)
}

####
# create statistics from an input vector of characters
####
corpStats <- function (rawchar) {
  chars <- nchar(rawchar)
  #  words <- length(rawchar)
  return(data.frame(
    lines = length(rawchar),  
    ave.char = mean(chars),
    var.char = var(chars)
  ))
}

####
# select a random sample of x% (default is 10) observations from an input vector of characters
####
subsample <- function(rawdata, percent=.1, seed=17071978) {
  set.seed(seed)
  len <- length(rawdata)
  idx <- rbinom(len, 1, prob=percent)
  return(rawdata[idx==1])
}


####
# Clean up a corpus
####
cleanCorp <- function (corp) {
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, stemDocument)
  return(corp)
}


####
# count term frequencies for a particular term document matrix
####
countTermFreq <- function(tdm) {
  m <- as.matrix(tdm)
  terms <- sort(rowSums(m),decreasing=TRUE)
  term.freq <- data.frame(term = names(terms),freq=terms)
  return(term.freq)
}