#####
# generate ngram models based on term frequencies for various coverage
####
require(compiler)
enableJIT(3)  # to force compile even nested functions

source('~/GitHub/jh_capstone/predict_lib.R')

outpath <- "dictionaries"
reload.tf <- TRUE

ptm <- proc.time()
compNgramSeqCount <- cmpfun(ngramSeqCount)
print(proc.time()-ptm)

coverage <- c(seq(.7, .8, .1), .9, .95, .99)  # 1 is 100%
#coverage <- .6

for (x in coverage) {
  print(x)
  if (reload.tf) {
    print("reload")
    ptm <- proc.time()
    loadTF(x)
    print(proc.time()-ptm)
    load(sprintf("dictionaries/tf4/tf4_%03.0f.RData", x*1000), .GlobalEnv)
    print(proc.time()-ptm)
  }
  ptm <- proc.time()
  uniModelC <- compNgramSeqCount(tf2)
  print(proc.time()-ptm)
  save(uniModelC, file=file.path(outpath, sprintf("unigramC_%03.0f.RData", x*1000)))
  print(proc.time()-ptm)
  
  biModelC <- compNgramSeqCount(tf3)
  print(proc.time()-ptm)
  save(biModelC, file=file.path(outpath, sprintf("bigramC_%03.0f.RData", x*1000)))
  print(proc.time()-ptm)
  
  triModelC <- compNgramSeqCount(tf4)
  print(proc.time()-ptm)
  save(triModelC, file=file.path(outpath, sprintf("trigramC_%03.0f.RData", x*1000)))
  print(proc.time()-ptm)
  
  save(uniModelC, biModelC, triModelC, 
       file=file.path(outpath, sprintf("ngramC_%03.0f.RData", x*1000)))
}