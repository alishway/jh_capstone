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
  names(val) <- NULL  # save memory
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
    print(proc.time()-ptm)  #1
  }

  #prep holdout words
  all.phr <- strsplit(target.holdout$phrase, " ")
  a.len <- sapply(all.phr, length)
  print(proc.time()-ptm)  #2
  
  ### Use only those with phrase length of at least 3 words to train, otherwise it will skew against trigrams
  #phr.idx <- a.len > 2
  phr.idx <- seq(length(a.len))
  phr <- strsplit(target.holdout$phrase[phr.idx], " ")
  len <- a.len[phr.idx]
  
  pre.uni <- mapply("[", phr, len)
  post.uni <- target.holdout$target[phr.idx]
  print(proc.time()-ptm)  #3

  #use mapply instead of paste to handle empty characters better
  pre.bi <- mapply(paste, mapply("[", phr, len-1), pre.uni)
  post.bi <- paste(pre.uni, post.uni)
  print(proc.time()-ptm)  #4
  
  pre.tri <- mapply(paste, mapply("[", phr, len-2), pre.bi)
  post.tri <- paste(pre.bi, post.uni)
  print(proc.time()-ptm)  #5
  
  #remove all n-grams with less than n tokens; marked by starting with space
  pre.bi <- gsub("^ .+", "", pre.bi)
  pre.tri <- gsub("^ .+", "", pre.tri)
  post.tri <- gsub("^ .+", "", post.tri)
  print(proc.time()-ptm)  #6

for (x in coverage) {
    print(x)
    ptm <- proc.time()

    if (reload.prob) {
      print("reload probabilities")
      load(file.path(setpath, sprintf("dictionaries/probC_%03.0f.RData", x*1000)))
      print(proc.time()-ptm)  #1
    }  
    
    #scale probs down so max is 1; to check if the scale affects lambda calculation -> it DOES
    prob.uni$prob <- prob.uni$prob / 255
    prob.bi$prob <- prob.bi$prob / 255
    prob.tri$prob <- prob.tri$prob / 255

    pre.uni.i <- parSapply(cl, pre.uni, compProbIdx, prob=prob.uni)
    print(proc.time()-ptm)  #2
    
    post.uni.i <- parSapply(cl, post.uni, compProbIdx, prob=prob.uni)
    print(proc.time()-ptm)  #3
    
    output.y <- pre.uni.i!=0 & post.uni.i!=0
    pr.uni <- numeric(length(pre.uni.i))
    pr.uni[output.y] <- prob.uni$prob[cbind(pre.uni.i, post.uni.i)[output.y, ]]
    print(proc.time()-ptm)  #4
    

    # retrieve probability for all pairs of pre- and post- bigram
    print("bigram")
    pre.bi.i <- parSapply(cl, pre.bi, compProbIdx, prob=prob.bi)
    print(proc.time()-ptm)  #5
    
    post.bi.i <- parSapply(cl, post.bi, compProbIdx, prob=prob.bi)
    print(proc.time()-ptm)  #6

    output.y <- pre.bi.i!=0 & post.bi.i!=0
    pr.bi <- numeric(length(pre.bi))
    pr.bi[output.y] <- prob.bi$prob[cbind(pre.bi.i, post.bi.i)[output.y, ]]
    print(proc.time()-ptm)  #7
    
    # retrieve probability for all pairs of pre- and post- trigram
    print("trigram")
    pre.tri.i <- parSapply(cl, pre.tri, compProbIdx, prob=prob.tri)
    print(proc.time()-ptm)  #8
    
    post.tri.i <- parSapply(cl, post.tri, compProbIdx, prob=prob.tri)
    print(proc.time()-ptm)  #9

    output.y <- pre.tri.i!=0 & post.tri.i!=0
    pr.tri <- numeric(length(pre.tri))
    pr.tri[output.y] <- prob.tri$prob[cbind(pre.tri.i, post.tri.i)[output.y, ]]
    print(proc.time()-ptm)  #10

    #combine all ngram probability to cross multiply with lambda
    pr.all <- rbind(pr.uni, pr.bi, pr.tri)
    
    # create all combination of l1*p1 + l2*p2 + l3*p3
    lambda.sum <- do.call(rbind, parLapply(cl, as.data.frame(t(lambda.set)),
                                        function (x, p) {return(colSums(x * p))}, pr.all))
    print(proc.time()-ptm)  #11
    
    lambda.sigma <- rowSums(lambda.sum)

    save(pr.uni, pr.bi, pr.tri, lambda.sum, lambda.set, lambda.sigma,
     file=file.path(setpath, sprintf("dictionaries/lambda_table_C%03.0f.RData", x*1000)))

#    save(pr.uni, pr.bi, pr.tri, pr.all,
#         file=file.path(setpath, sprintf("dictionaries/lambda_table_C%03.0f.RData", x*1000)))
    print(proc.time()-ptm)  #12
    
    print("Completed")
  }
#}

#compGenerateLambda <- cmpfun(generateLambda)

#compGenerateLambda(coverage)

stopCluster(cl)
