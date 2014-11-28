qn <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
        "Hey sunshine, can you follow me and make me the",
        "Very early observations on the Bills game: Offense still struggling but the",
        "Go on a romantic date at the",
        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
        "Be grateful for the good times and keep the faith during the",
        "If this isn't the cutest thing you've ever seen, then you must be")

require(compiler)
enableJIT(1)  # to force compile even nested functions

source('~/GitHub/jh_capstone/predict_lib.R')
compNgramSeqCount <- cmpfun(ngramSeqCount)

corp <- VCorpus(VectorSource(qn))
c.corp <- cleanCorp(corp)

#load("dictionaries/ngramC_700.RData")

predNgramBackoff <- function (phrase, n, model) {
  corp <- VCorpus(VectorSource(phrase))
  corp <- cleanCorp(corp)
  wordlist <- unlist(strsplit(corp[[1]]$content, " "))
  len <- length(wordlist)
  if (len <= n) {
    print("phrase shorter than n-gram size, try using bigger n-gram model")
    pred <- NA
  }
  else {
    preterm <- paste(wordlist[(len-n+1):len], collapse=" ")
    maxrow <- which(model$terms == preterm)
    # TO DO: How to handle maxrow returning integer(0) (i.e. term doesn't exist)
    maxcol <- which(model$counts[maxrow, ] == max(model$counts[maxrow,]))
    pred <- model$terms[maxcol]
  }
  
  return(pred)
}

n <- 2
model <- biModelC

for (x in seq_along(qn)) {
  ptm <- proc.time()
  pred <- predNgramBackoff(qn[x], n, model)
  print(pred)
  print(proc.time()-ptm)
}
