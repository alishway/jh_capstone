# create statistics of corpuses
###################

setwd("~/GitHub/jh_capstone")
source('~/GitHub/jh_capstone/JHCap_lib.R')

outpath <- "rawdata"

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


#load(file.path(outpath, "rawdata.RData"))

# if no variables "blogs", "news", and "twits",
# source('~/GitHub/jh_capstone/load_corpus.R')  to read into the above

x<-.006
#for (x in seq(.002, .02, .002)) {
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
  
  corp <- cleanCorp(s.corp)
  print(proc.time()-ptm)
  
  tdm <- TermDocumentMatrix(corp)
  tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
  tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
#  m <- as.matrix(tdm)
#  terms <- sort(rowSums(as.matrix(tdm)),decreasing=TRUE)
  terms <- sort(rowSums(inspect(tdm[, dimnames(tdm)$Docs])), decreasing=TRUE)
  term.freq <- data.frame(term = names(terms),freq=terms)
  
#  term2.freq <- countTermFreq(tdm2)
#  term3.freq <- countTermFreq(tdm3)
  print(proc.time()-ptm)

  corp.stats <- corpStats(s)
  corp.stats$ave.terms <- mean(terms)
  corp.stats$var.terms <- var(terms)
  save(corp.stats, file = file.path(subdir, "stats.RData"))
  save(s, file = file.path(subdir, "rawchar.RData"))
  save(corp, file = file.path(subdir, "cleaned_corpus.RData"))
  save(tdm, term.freq, file = file.path(subdir, "tdm_n1.RData"))
  save(tdm2, file = file.path(subdir, "tdm_n2.RData"))
  save(tdm3, file = file.path(subdir, "tdm_n3.RData"))
  print(proc.time()-ptm)
  print("complete")
#}