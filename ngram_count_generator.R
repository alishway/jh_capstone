#####
# generate ngram models based on term frequencies for various coverage
####


source('~/GitHub/jh_capstone/predict_lib.R')

outpath <- "dictionaries"
reload.tf <- TRUE

coverage <- c(seq(.5, .8, .1), seq(.9, .99, .01))  # 1 is 100%
coverage <- .5

for (x in coverage) {
  print(x)
  if (reload.tf) {
    print("reload")
    ptm <- proc.time()
    loadTF(coverage)
    print(proc.time()-ptm)
    load(sprintf("dictionaries/tf4/tf4_%03.0f.RData", coverage*1000), .GlobalEnv)
    print(proc.time()-ptm)
  }
  ptm <- proc.time()
  uniModelC <- ngramSeqCount(tf1, tf2)
  print(proc.time()-ptm)
  biModelC <- ngramSeqCount(tf2, tf3)
  print(proc.time()-ptm)
  triModelC <- ngramSeqCount(tf2, tf3)
  print(proc.time()-ptm)
  
  save(uniModelC, biModelC, triModelC,
       file=file.path(outpath, sprintf("ngramC_%03.0f.RData", x*1000)))
  print(proc.time()-ptm)
}