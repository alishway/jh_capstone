### library of functions for JH Capstone project on NLP

require(tm)
require(RWeka)
require(Matrix)

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
  rw <- unlist(strsplit(rawchar, " "))
  rough.words <- length(rw)
  rough.unique.words <- length(unique(rw))
  
  return(data.frame(
    lines = length(rawchar),
    chars = sum(chars),
    ave.char = mean(chars),
    var.char = var(chars),
    rough.words = length(rw),
    rough.unique.words = length(unique(rw)),
    ratio.unique.words = rough.unique.words/rough.words,
    ave.rough.words = rough.words/length(rawchar)
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
  m <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v)
  term.freq <- sort(rowSums(m),decreasing=TRUE)
  return(term.freq)
}