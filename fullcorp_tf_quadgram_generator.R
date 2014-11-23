source('~/GitHub/jh_capstone/JHCap_lib.R')

reloadRaw <- FALSE  # not needed if reloading from existing corpus, see below

if (reloadRaw) {
  ptm <- proc.time()  # start timer
  load("rawdata/rawdata.RData")
  print(proc.time()-ptm)
  
  full <- c(twits, blogs, news)
  rm(twits, blogs, news)
  print(proc.time()-ptm)
}

cu <- cut(1:length(full), 100, labels=FALSE)

outpath <- "_fullcorp"

for (x in (5:length(unique(cu)))) {
  print(x)
  ptm <- proc.time()  # start timer
  subdir <- file.path(outpath, sprintf("%03.0f", x))

  # assumption is cleaning of corpus already done before by fullcorp_tf_generator.R
  load(file.path(subdir, "cleaned_corpus.RData"))
  print(proc.time()-ptm)
  
  tdm4 <- TermDocumentMatrix(corp, control = list(tokenize = QuadgramTokenizer))
  save(tdm4, file = file.path(subdir, "tdm_n4.RData"))
  tf4 <- countTermFreq(tdm4)
  rm(tdm4)  # immediate remove to save memory
  print(proc.time()-ptm)
  
  save(tf4, file = file.path(subdir, "tf4.RData"))
  
  print(proc.time()-ptm)
  print("complete")
}