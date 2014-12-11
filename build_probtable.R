#####
# create probability table from n-gram models (sparse matrix)
####

source('~/GitHub/jh_capstone/predict_lib.R')

coverage <- .5
reload.tf <- FALSE
setpath <- "dictionaries"
coverage <- .7

if (reload.tf) {
  ptm <- proc.time()
  load(file.path(setpath, sprintf("ngramC_%03.0f.RData", coverage*1000)))
  print(proc.time()-ptm)
}

#load model as unigram/bigram/trigram
#model <- uniModelC
#d <- .75  # discount value for counts
buildProbNgram <- function (model, d = .75) {
  prob <- model$counts
  
  #minus discount only for those with non-zero counts, to avoid
  #the sparse matrix becomes non-sparse.
  prob@x <- prob@x - d
  
  prob <- prob/
    ifelse(rowSums(prob)==0, 1, rowSums(prob))
  
  #quantise between 0-255 (8-bit)
  prob@x <- floor(prob@x*255)
  
  return(list(terms=model$terms, prob=prob))
}

ptm <- proc.time()
prob.uni <- buildProbNgram(uniModelC)
print(proc.time()-ptm)

#prob.bi <- buildProbNgram(biModelC)
print(proc.time()-ptm)

#prob.tri <- buildProbNgram(triModelC)
print(proc.time()-ptm)

#save(prob.uni, prob.bi, prob.tri,
#     file=file.path(setpath, sprintf("probC_%03.0f.RData", coverage)))