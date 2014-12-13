####
# search for optimum lambda weights for prediction
####

source('~/GitHub/jh_capstone/predict_lib.R')
require(parallel)

cl <- makePSOCKcluster(4)

####
# convenience function to generate all possible combinations of value
# of d columns with values ranging between a and b in increments of x
# where the sigma of the values in the combination cannot exceed l
# Sample application: to generate lambda 1, lambda 2, lambda 3
# where all lambda are between 0 to 1 (0, 0.5, and 1)
# and constraint sigma lambda is equal to 1
####

lambdaComb <- function(a=0, b=1, x=.5, d=3, l=1) {
  m.seq <- seq(a, b, x)
  #m <- expand.grid(l1 = m.seq, l2 = m.seq, l3=m.seq)
  m <- do.call(expand.grid, as.data.frame(matrix(rep(m.seq, d), ncol=d)))
#  m <- m[rowSums(m)==l, ]
  m <- as.matrix(m[rowSums(m)==l, ])
#  dimnames(m) <- NULL
  return(m)
}

###
# convenience function for cutting a phrase to length for n-gram model
# returning the preceding ngram and the following ngram
###
cutPhrase <- function(phrase, n) {
  words <- unlist(strsplit(phrase, " "))
  len <- length(words)
  if (len <= n) return(list())  # not long enough for ngram, need at least n+1 words
  
  pre <- paste(words[(len-n):(len-1)], collapse=" ")
  post <- paste(words[(len-n+1):len], collapse=" ")
  
  return(list(pre=pre, post=post))
}
compCutPhrase <- cmpfun(cutPhrase)

###
# convenience function for checking probability of a n-gram model, without error checking
###
chkMod <- function(preterm, postterm, model) {
  pre <- which(model$terms==preterm)
  post <- which(model$terms==postterm)
  
  return(model$prob[pre, post])
}
compChkMod <- cmpfun(chkMod)

###
# convenience function for checking probability of a trigram model
###
chkTri <- function(phrase, tri=prob.tri) {
  terms <- cutPhrase(phrase, 3)
  if (length(terms)==0) return(0)  # if too short for trigram
  pre <- which(tri$terms==terms$pre)
  post <- which(tri$terms==terms$post)
  
  return(tri$prob[pre, post])
}

###
# convenience function for checking index of an ngram in the model, for use in apply
###
probIdx <- function (x, prob) {
  idx <- which(prob$terms == x)
  if (length(idx) == 0) idx <- 0
#  names(idx) <- NULL  # save memory
  return(idx)
}
compProbIdx <- cmpfun(probIdx)


###
# convenience function for checking probability of an ngram-pair in the model, for use in apply
###
probVal <- function (x, y, prob) {
  val <- prob$prob[x, y]
  if (length(val) == 0) val <- 0
#  names(val) <- NULL  # save memory
  return(val)
}
compProbVal <- cmpfun(probVal)

reload.prob <- TRUE
reload.holdout <- TRUE
lambda.set <- lambdaComb(0, 1, .1, 3, 1)
coverage <- c(seq(.7, .95, .05), .99)  # 1 is 100%
#coverage <- .7
train.size <- .7

#####
# main function to generate lambda for various coverage (to be compiled)
#####
#generateLambda <- function(coverage) {
  ptm <- proc.time()
  setpath <- file.path("training", sprintf("%03.0f", train.size*100))
  if (reload.holdout) {
    print("reload holdout")
    load(file.path(setpath, "target_words_holdout.RData"))
    print(proc.time()-ptm)
  }
  for (x in coverage) {
    print(x)
    ptm <- proc.time()

    if (reload.prob) {
      print("reload probabilities")
      load(file.path(setpath, sprintf("dictionaries/probC_%03.0f.RData", x*1000)))
      print(proc.time()-ptm)
    }  
    
    #scale probs down so max is 1; to check if the scale affects lambda calculation -> it DOES
    prob.uni$prob <- prob.uni$prob / 255
    prob.bi$prob <- prob.bi$prob / 255
    prob.tri$prob <- prob.tri$prob / 255
    
    phr <- strsplit(target.holdout$phrase, " ")
    len <- sapply(phr, length)
    print(proc.time()-ptm)
    
    pre.uni <- mapply("[", phr, len)
    post.uni <- target.holdout$target
    print(proc.time()-ptm)
    
    pre.uni.i <- parSapply(cl, pre.uni, compProbIdx, prob=prob.uni)
    print(proc.time()-ptm)
    
    post.uni.i <- parSapply(cl, post.uni, compProbIdx, prob=prob.uni)
    print(proc.time()-ptm)
    
    print("enter parallel")
    # retrieve probability for all pairs of pre- and post- unigram
    pr.uni <- mapply(compProbVal, pre.uni.i, post.uni.i,
                     MoreArgs = list(prob = prob.uni), USE.NAMES=FALSE)
#    pr.uni <- clusterMap(cl, compProbVal, pre.uni.i, post.uni.i,
#                         MoreArgs = list(prob = prob.uni), USE.NAMES=FALSE)
    print(proc.time()-ptm)
    rm(pre.uni.i, post.uni.i)
    
    #use mapply instead of paste to handle empty characters better
    pre.bi <- mapply(paste, mapply("[", phr, len-1), pre.uni)
    post.bi <- paste(pre.uni, post.uni)
    print(proc.time()-ptm)
    
    pre.tri <- mapply(paste, mapply("[", phr, len-2), pre.bi)
    post.tri <- paste(pre.bi, post.uni)
    print(proc.time()-ptm)

    #remove to save memory
    rm(phr)
    gc()
    
    #remove all n-grams with less than n tokens; marked by starting with space
    pre.bi <- gsub("^ .+", "", pre.bi)
    pre.tri <- gsub("^ .+", "", pre.tri)
    post.tri <- gsub("^ .+", "", post.tri)
    print(proc.time()-ptm)

    #remove to save memory
    rm(post.uni, pre.uni)
    gc()

    
    # retrieve probability for all pairs of pre- and post- bigram
    pre.bi.i <- parSapply(cl, pre.bi, compProbIdx, prob=prob.bi)
    print(proc.time()-ptm)
    
    post.bi.i <- parSapply(cl, post.bi, compProbIdx, prob=prob.bi)
    print(proc.time()-ptm)

    pr.bi <- mapply(compProbVal, pre.bi.i, post.bi.i,
                    MoreArgs = list(prob = prob.bi), USE.NAMES=FALSE)
#   pr.bi <- clusterMap(cl, compProbVal, pre.bi.i, post.bi.i,
#                    MoreArgs = list(prob = prob.bi), USE.NAMES=FALSE)
    print(proc.time()-ptm)

    rm(pre.bi.i, post.bi.i)

    #remove to save memory
    rm(pre.bi, post.bi)
    gc()
    
    # retrieve probability for all pairs of pre- and post- trigram
    pre.tri.i <- parSapply(cl, pre.tri, compProbIdx, prob=prob.tri)
    print(proc.time()-ptm)
    
    post.tri.i <- parSapply(cl, post.tri, compProbIdx, prob=prob.tri)
    print(proc.time()-ptm)
    
    pr.tri <- mapply(compProbVal, pre.tri.i, post.tri.i,
                     MoreArgs = list(prob = prob.tri), USE.NAMES=FALSE)
#   pr.tri <- clusterMap(cl, compProbVal, pre.tri.i, post.tri.i,
#                     MoreArgs = list(prob = prob.tri), USE.NAMES=FALSE)
    print(proc.time()-ptm)

    #remove to save memory
    rm(pre.tri.i, post.tri.i)
    rm(pre.tri, post.tri)
    rm(prob.uni, prob.bi, prob.tri)
    gc()
    print(proc.time()-ptm)

    #combine all ngram probability to cross multiply with lambda
    pr.all <- rbind(pr.uni, pr.bi, pr.tri)
    
    # create all combination of l1*p1 + l2*p2 + l3*p3
    lambda.sum <- do.call(rbind, parLapply(as.data.frame(t(lambda.set)),
                                        function (x) {return(colSums(x * pr.all))}))
    print(proc.time()-ptm)
    
    lambda.sigma <- rowSums(lambda.sum)
    
    save(pr.uni, pr.bi, pr.tri, lambda.sum, lambda.set, lambda.sigma,
         file=file.path(setpath, sprintf("dictionaries/lambda_table_C%03.0f.RData", x*1000)))
    print(proc.time()-ptm)
    
    print("Completed")
  }
#}

#compGenerateLambda <- cmpfun(generateLambda)

#compGenerateLambda(coverage)

stopCluster(cl)
