# create statistics of corpuses
###################

setwd("~/GitHub/jh_capstone")
source('~/GitHub/jh_capstone/JHCap_lib.R')

require(Matrix)

outpath <- "rawdata"

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
hashtag.pattern <- "#[0-9a-zA-Z]+"

#load(file.path(outpath, "rawdata.RData"))

# if no variables "blogs", "news", and "twits",
# source('~/GitHub/jh_capstone/load_corpus.R')  to read into the above

#x<-.006
for (x in seq(.004, .006, .002)) {
  print(x)
  ptm <- proc.time()  # start timer
  subdir <- file.path(outpath, sprintf("%03.0f", 1000*x))
  dir.create(subdir, showWarnings = FALSE)
  
  s.twits <- subsample(twits, x)
  s.blogs <- subsample(blogs, x)
  s.news <- subsample(news, x)
  
  s <- c(s.twits, s.blogs, s.news)
  rm(s.twits, s.blogs, s.news)
  
  #cleanup unwanted words e.g. hashtag

  ht <- regmatches(s, gregexpr(hashtag.pattern, s))
  
  s.corp <- VCorpus(VectorSource(gsub(hashtag.pattern, "", s)))

  corp.stats <- corpStats(s)
  
  save(s, file = file.path(subdir, "rawchar.RData"))
  rm(s)  # immediate remove to save memory
  
  corp <- cleanCorp(s.corp)
  rm(s.corp)  # immediate remove to save memory
  print(proc.time()-ptm)
  
  tdm <- TermDocumentMatrix(corp)


#  m <- as.matrix(tdm)

  capture.output(terms <- sort(rowSums(inspect(tdm[, dimnames(tdm)$Docs])), decreasing=TRUE))
#  term.freq <- data.frame(term = names(terms),freq=terms)
#  term2.freq <- countTermFreq(tdm2)
#  term3.freq <- countTermFreq(tdm3)

  print(proc.time()-ptm)

  save(corp.stats, terms, file = file.path(subdir, "stats.RData"))
  save(corp, file = file.path(subdir, "cleaned_corpus.RData"))
  save(tdm, file = file.path(subdir, "tdm_n1.RData"))
  rm(tdm, terms)  # immediate remove to save memory

  tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
  save(tdm2, file = file.path(subdir, "tdm_n2.RData"))
  rm(tdm2)  # immediate remove to save memory

  tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
  save(tdm3, file = file.path(subdir, "tdm_n3.RData"))
  rm(tdm3)  # immediate remove to save memory
  
  print(proc.time()-ptm)
  print("complete")
}