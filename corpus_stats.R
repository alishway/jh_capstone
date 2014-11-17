# create statistics of corpuses
###################

setwd("~/GitHub/jh_capstone")
source('~/GitHub/jh_capstone/JHCap_lib.R')

require(Matrix)

outpath <- "rawdata"

hashtag.pattern <- "#[0-9a-zA-Z]+"

#load(file.path(outpath, "rawdata.RData"))

# if no variables "blogs", "news", and "twits",
# source('~/GitHub/jh_capstone/load_corpus.R')  to read into the above

#idx.seq <- .002
idx.seq <- c(.01, seq(.05, .25, .05))

for (x in idx.seq) {
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
  s.corp <- VCorpus(VectorSource(gsub(hashtag.pattern, "", s)))
  corp.stats <- corpStats(s)
  print(proc.time()-ptm)
  
  save(s, file = file.path(subdir, "rawchar.RData"))
  rm(s)  # immediate remove to save memory
  
  corp <- cleanCorp(s.corp)
  rm(s.corp)  # immediate remove to save memory
  print(proc.time()-ptm)
  
  tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(1,Inf)))
  print(proc.time()-ptm)

#  save(corp.stats, terms, file = file.path(subdir, "stats.RData"))
  save(corp.stats, file = file.path(subdir, "stats.RData"))
  save(corp, file = file.path(subdir, "cleaned_corpus.RData"))
  save(tdm, file = file.path(subdir, "tdm_n1.RData"))
  tf1 <- countTermFreq(tdm)
  rm(tdm)  # immediate remove to save memory
  print(proc.time()-ptm)

  tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
  save(tdm2, file = file.path(subdir, "tdm_n2.RData"))
  tf2 <- countTermFreq(tdm2)
  rm(tdm2)  # immediate remove to save memory
  print(proc.time()-ptm)

  tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
  save(tdm3, file = file.path(subdir, "tdm_n3.RData"))
  tf3 <- countTermFreq(tdm3)
  rm(tdm3)  # immediate remove to save memory
  print(proc.time()-ptm)

  save(tf1, tf2, tf3, file = file.path(subdir, "tf.RData"))

  print(proc.time()-ptm)
  print("complete")
}