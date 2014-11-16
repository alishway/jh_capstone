# create statistics of corpuses
###################

setwd("~/GitHub/jh_capstone")
source('~/GitHub/jh_capstone/JHCap_lib.R')

require(Matrix)

outpath <- "rawdata"

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
hashtag.pattern <- "#[0-9a-zA-Z]+"


  ptm <- proc.time()  # start timer
  subdir <- file.path(outpath, "full")
  dir.create(subdir, showWarnings = FALSE)

  
  s <- c(twits, blogs, news)
  print(proc.time()-ptm)

  full.corp.stats <- corpStats(s)
  twit.stats <- corpStats(twits)
  news.stats <- corpStats(news)
  blog.stats <- corpStats(blogs)
  save(full.corp.stats, twit.stats, news.stats, blog.stats,
       file=file.path(subdir, "stats.RData"))
  print(proc.time()-ptm)

  #cleanup unwanted words e.g. hashtag
  s.corp <- VCorpus(VectorSource(gsub(hashtag.pattern, "", s)))
  print(proc.time()-ptm)

  save(s, file = file.path(subdir, "rawchar.RData"))
    print(proc.time()-ptm)

  save(s.corp, file = file.path(subdir, "cleaned_corpus.RData")
  print(proc.time()-ptm)
  print("complete")
