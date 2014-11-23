source('~/GitHub/jh_capstone/JHCap_lib.R')

#load("rawdata/rawdata.RData")
full <- c(twits, blogs, news)

cu <- cut(1:length(full), 100, labels=FALSE)

outpath <- "_fullcorp"

for (x in (81:length(unique(cu)))) {
  print(x)
  ptm <- proc.time()  # start timer
  subdir <- file.path(outpath, sprintf("%03.0f", x))
  dir.create(subdir, showWarnings = FALSE)
  
  #process just one percentile of corpora
  #cleanup unwanted words e.g. hashtag 
  s.corp <- VCorpus(VectorSource(gsub(hashtag.pattern, "", full[cu==x])))
  print(proc.time()-ptm)
  
  corp <- cleanCorp(s.corp)
  rm(s.corp)  # immediate remove to save memory
  save(corp, file = file.path(subdir, "cleaned_corpus.RData"))
  print(proc.time()-ptm)
  
  tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(1,Inf)))
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