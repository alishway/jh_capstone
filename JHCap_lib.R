### library of functions for JH Capstone project on NLP

require(compiler)
enableJIT(3)  # to force compile even nested functions

require(tm)
require(RWeka)
require(Matrix)
require(slam)

# crucial to source with encoding for cleanup
source.with.encoding('~/GitHub/jh_capstone/cleanup_lib.R', encoding='UTF-8')

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
# select a random sample of x% (default is 10) observations from an input vector
# of characters, returning its index
####
subsample.idx <- function(rawdata,  percent=.1, seed=17071978) {
  set.seed(seed)
  len <- length(rawdata)
  idx <- rbinom(len, 1, prob=percent)
  return(which(idx==1))
}

####
# count term frequencies for a particular term document matrix and sort
####
countTermFreq <- function(tdm) {
  term.freq <- sort(row_sums(tdm),decreasing=TRUE)
  return(term.freq)
}

compCountTermFreq <- cmpfun(countTermFreq)


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

  wordlist <- unlist(strsplit(bigterm, " "))
  len <- length(wordlist)
  term1 <- paste(wordlist[1:(len-1)], collapse=" ")
  term2 <- paste(wordlist[2:len], collapse=" ")
  trow <- which(termlist==term1)
  tcol <- which(termlist==term2)
  return(c(trow, tcol))
}


####
# create n-gram model count, given term frequencies of n+1 gram
# returns n-gram model as sparseMatrix
# and the list of terms for the row and col of sparseMatrix
####
ngramSeqCount <- function(tf2) {
  timing <- FALSE  # turn TRUE for time bottleneck debugging
  
  if (timing) print("Start")
  ptm <- proc.time()
  
  n.tf2 <- names(tf2)  #n+1 terms
  if (timing) print(proc.time()-ptm)  #1
  
  
  # construct possible n terms from n+1 terms
  #n.tf1 <- sort(unique(unlist(strsplit(n.tf2, " "))))
  u.tf2 <- strsplit(n.tf2, " ")
  if (timing) print(proc.time()-ptm)  #2
  
  len <- length(u.tf2[[1]])  # assumption is all have same number of chars as first element
  if (timing) print(proc.time()-ptm)  #3
  
  if (len < 2) {
    print("term frequencies have to be of terms of at least 2 words (bigram) length")
    return(NA)
  }
  
  n.tf1 <- do.call(c, lapply(u.tf2, function (x)
  {return(c(paste(x[1:(len-1)], collapse = " "),
            paste(x[2:len], collapse = " ")))}))
  if (timing) print(proc.time()-ptm)  #4
  
  n.tf1 <- sort(unique(n.tf1))
  if (timing) print(proc.time()-ptm)  #5
  
  #map all the "coordinates" of preceding and following terms

  wn.tf2 <- strsplit(n.tf2, " ")
  len <- length(wn.tf2[[1]])  # Assumption: all elements in list have same length
  term1 <- sapply(wn.tf2, function(x) paste(x[1:(len-1)], collapse=" "))
  term2 <- sapply(wn.tf2, function(x) paste(x[2:len], collapse=" "))
  
  trow <- match(term1, n.tf1)
  tcol <- match(term2, n.tf1)

  if (timing) print(proc.time()-ptm)  #6
  
  counts <- sparseMatrix(trow, tcol, x=as.vector(tf2))
  if (timing) print(proc.time()-ptm)  #7
  
  if (timing) print("Complete")
  
  #return as list of term list and sparsematrix
  return(list(terms = n.tf1,
              counts = counts))
}

compNgramSeqCount <- cmpfun(ngramSeqCount)

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