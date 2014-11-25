### library of functions for JH Capstone project on NLP

require(tm)
require(RWeka)
require(Matrix)
require(slam)

outpath <- "rawdata"
hashtag.pattern <- "#[0-9a-zA-Z]+"

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

####
# create a corpus from raw character and tagging origin
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
# designed for subfolders in folder "rawdata" in mind
# where the subfolder number is the per-thousandth sample of full corpus
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
# CAUTION: THIS FUNCTION ASSUMES TF HAS BEEN SORTED IN ASCENDING ORDER
calcFreqCutoff <- function(tf, coverage=.9) {
  cumtf <- cumsum(tf)/sum(tf)
  c.idx <- length(which(cumtf<=coverage)) + 1  # plus 1 to ensure cumulative sum exceeds coverage
  return(c.idx)
}


####
# decompose an n-gram into a sequence of two n-1 grams, and give the sequence in termlist,
# where row is the sequence number of the preceding n-1 gram
# and col is the sequence number of the following n-1 gram
####
coordTF <- function(bigterm, termlist) {
  words <- unlist(strsplit(bigterm, " "))
  len <- length(words)
  term1 <- paste(words[1:(len-1)], collapse=" ")
  term2 <- paste(words[2:len], collapse=" ")
  trow <- which(termlist==term1)
  tcol <- which(termlist==term2)
  return(c(trow, tcol))
}


####
# create n-gram model count, given term frequencies of n and n+1 gram
# (tf1 and tf2 respectively)
# returns a sparseMatrix
####
ngramSeqCount <- function(tf1, tf2) {
  n.tf1 <- names(tf1)
  n.tf2 <- names(tf2)
  
  #map all the "coordinates" of preceding and following terms
  map.tf <- sapply(n.tf2, coordTF, n.tf1)
  # TO DO: catch rows and cols returned as zero: filter out from mapping matrix
  
  #return as sparsematrix
  return(sparseMatrix(map.tf[1, ], map.tf[2, ], x=tf2))
}


####
# calculate index for last word to reach frequency coverage 
####
buildProbMatrix <- function(n=1, coverage=.5, sourcepath="dictionaries") {
  if (n > 3) {
    print("at the moment only handles up to trigram")
    return(NA)
  }
  load(file.path(sourcepath, paste0("tf_", coverage*1000, ".RData")))
  if (n==2)
    load(file.path(sourcepath, paste0("tf4/tf4_", coverage*1000, ".RData")))
  
  
}