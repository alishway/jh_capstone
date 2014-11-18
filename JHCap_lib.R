### library of functions for JH Capstone project on NLP

require(tm)
require(RWeka)
require(Matrix)
require(slam)

outpath <- "rawdata"

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

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
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removePunctuation,
                 preserve_intra_word_dashes=TRUE)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, stripWhitespace)
  return(corp)
}


####
# count term frequencies for a particular term document matrix and sort
####
countTermFreq <- function(tdm) {
  term.freq <- sort(row_sums(tdm),decreasing=TRUE)
  return(term.freq)
}



####
# load all data from a particular folder 
####
loadData <- function(pathname) {
  subdir <- file.path(outpath, pathname)
  load(file.path(subdir, "rawchar.RData"), .GlobalEnv)
  load(file.path(subdir, "stats.RData"), .GlobalEnv)
  load(file.path(subdir, "cleaned_corpus.RData"), .GlobalEnv)
  load(file.path(subdir, "tdm_n1.RData"), .GlobalEnv)
  load(file.path(subdir, "tf.RData"), .GlobalEnv)
}


####
# calculate index for last word to reach frequency coverage 
####
calcFreqCutoff <- function(tf, coverage=.9) {
  cumtf <- cumsum(tf)/sum(tf)
  c.idx <- length(which(cumtf<=coverage)) + 1  # plus 1 to ensure cumulative sum exceeds coverage
  return(c.idx)
}