#####
# create probability table from n-gram models (sparse matrix)
####

source('~/GitHub/jh_capstone/predict_lib.R')

coverage <- .5
reload.tf <- TRUE
setpath <- "dictionaries/training/070"
coverage <- .7

if (reload.tf) {
  ptm <- proc.time()
  load(file.path(setpath, sprintf("ngramC_%03.0f.RData", coverage)))
  print(proc.time()-ptm)
}

#load model as unigram/bigram/trigram
#model <- uniModelC
#d <- .75  # discount value for counts
buildProbNgram <- function (model, d=.75) {
  # TO DO: error check to prevent n being incompatible with tf
  # (i.e. n=1 must be unigram model tf)
  
  #eliminate tokens with 0 count
  zeros <- rowSums(model$counts) == 0
  prob.model <- model$counts[-zeros, -zeros]
  terms <- model$terms[-zeros]

  prob.model <- (prob.model-d)/rowSums(prob.model)
  return(list(terms=terms, prob=prob.model)
}

prob.uni <- buildProbNgram(uniModelC)
prob.bi <- buildProbNgram(biModelC)
prob.tri <- buildProbNgram(triModelC)

save(prob.uni, prob.bi, prob.tri,
     file=file.path(setpath, sprintf("probC_%03.0f.RData", coverage)))