####
# search for optimum lambda weights for prediction
####

source('~/GitHub/jh_capstone/predict_lib.R')


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
#coverage <- c(seq(.7, .95, .05), .99)  # 1 is 100%
coverage <- .7
train.size <- .7


for (x in coverage) {
  setpath <- file.path("training", sprintf("%03.0f", train.size*100))
  print(x)
  ptm <- proc.time()
  
  if (reload.holdout) {
    print("reload holdout")
    load(file.path(setpath, "target_words_holdout.RData"))
    print(proc.time()-ptm)
  }

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
  print(proc.time()-ptm)
  
  pre.bi <- mapply(paste, mapply("[", phr, len-1), pre.uni)
  print(proc.time()-ptm)
  
  #use mapply instead of paste to handle empty characters better
  pre.tri <- mapply(paste, mapply("[", phr, len-2), pre.bi)
  print(proc.time()-ptm)
  
  post.uni <- target.holdout$target
  post.bi <- paste(pre.uni, post.uni)
  post.tri <- paste(pre.bi, post.uni)
  print(proc.time()-ptm)
  
  #remove all n-grams with less than n tokens; marked by starting with space
  pre.bi <- gsub("^ .+", "", pre.bi)
  pre.tri <- gsub("^ .+", "", pre.tri)
  post.tri <- gsub("^ .+", "", post.tri)
  print(proc.time()-ptm)
  
  # retrieve probability for all pairs of pre- and post- ngram
  pr.uni <- mapply(compProbVal, sapply(pre.uni, compProbIdx, prob=prob.uni),
                   sapply(post.uni, compProbIdx, prob=prob.uni),
                   MoreArgs = list(prob = prob.uni), USE.NAMES=FALSE)
  print(proc.time()-ptm)
  
  pr.bi <- mapply(compProbVal, sapply(pre.bi, compProbIdx, prob=prob.bi),
                  sapply(post.bi, compProbIdx, prob=prob.bi),
                  MoreArgs = list(prob = prob.bi), USE.NAMES=FALSE)
  print(proc.time()-ptm)
  
  pr.tri <- mapply(compProbVal, sapply(pre.tri, compProbIdx, prob=prob.tri),
                   sapply(post.tri, compProbIdx, prob=prob.tri),
                   MoreArgs = list(prob = prob.tri), USE.NAMES=FALSE)
  print(proc.time()-ptm)
  
  pr.all <- rbind(pr.uni, pr.bi, pr.tri)
  
  # create all combination of l1*p1 + l2*p2 + l3*p3
  lambda.sum <- do.call(rbind, lapply(as.data.frame(t(lambda.set)),
                                      function (x) {return(colSums(x * pr.all))}))
  print(proc.time()-ptm)
  
  lambda.sigma <- rowSums(lambda.sum)
  
  print("Completed")
}

