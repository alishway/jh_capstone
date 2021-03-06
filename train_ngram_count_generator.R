#####
# generate ngram models based on term frequencies for various coverage
####
require(compiler)
enableJIT(3)  # to force compile even nested functions

source('~/GitHub/jh_capstone/predict_lib.R')

outpath <- "training/070/dictionaries"
reload.tf <- FALSE

ptm <- proc.time()
compNgramSeqCount <- cmpfun(ngramSeqCount)
print(proc.time()-ptm)

coverage <- c(seq(.7, .95, .05), .99)  # 1 is 100%
#coverage <- .7
coverage <- 1

for (x in coverage) {
  print(x)
  if (reload.tf) {
    print("reload")
    ptm <- proc.time()
    load(file.path(outpath, sprintf("tf_%03.0f.RData", x*1000)), .GlobalEnv)
    print(proc.time()-ptm)
  }
  
  tf2 <- tf2[!is.na(tf2)]
  tf3 <- tf3[!is.na(tf3)]
  tf4 <- tf4[!is.na(tf4)]
  
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

#  load(file.path(outpath, sprintf("unigramC_%03.0f.RData", x*1000)))
#  load(file.path(outpath, sprintf("trigramC_%03.0f.RData", x*1000)))

  save(uniModelC, biModelC, triModelC, 
       file=file.path(outpath, sprintf("ngramC_%03.0f.RData", x*1000)))
}