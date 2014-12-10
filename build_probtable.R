source('~/GitHub/jh_capstone/predict_lib.R')

coverage <- .5
reload.tf <- TRUE
use.tf4 <- TRUE

if (reload.tf) {
  ptm <- proc.time()
#  loadTF(coverage)
  load("dictionaries/ngramC_700.RData")
  print(proc.time()-ptm)
#  if (use.tf4) {
#    load(sprintf("dictionaries/tf4/tf4_%03.0f.RData", coverage*1000), .GlobalEnv)
#    print(proc.time()-ptm)    
#  }
}

#load model as unigram/bigram/trigram
model <- uniModelC
d <- .75  # discount value for counts
#buildProbNgram <- function (model, d=.75) {
  # TO DO: error check to prevent n being incompatible with tf
  # (i.e. n=1 must be unigram model tf)
  
  #eliminate tokens with 0 count
  zeros <- rowSums(model$counts) == 0
  prob.model <- model$counts[-zeros, -zeros]
  terms <- model$terms[-zeros]

  prob.model <- (prob.model-d)/rowSums(prob.model)
  return(list(terms=terms, prob=prob.model)
#}

prob.UniModel <- buildProbNgram(uniModelC)
