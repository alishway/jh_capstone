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

source('~/GitHub/jh_capstone/predict_lib.R')

coverage <- .99
reload.tf <- TRUE
use.tf4 <- TRUE

if (reload.tf) {
  ptm <- proc.time()
  loadTF(coverage)
  print(proc.time()-ptm)
  if (use.tf4) {
    load(sprintf("dictionaries/tf4/tf4_%03.0f.RData", coverage*1000), .GlobalEnv)
    print(proc.time()-ptm)    
  }
}
if (use.tf4) {
  for (x in seq_along(qn)) {
    ptm <- proc.time()
    pred <- predSimpleBackoffQuad(qn[x], tf1, tf2, tf3, tf4)
    print(pred)
    print(proc.time()-ptm)
  } 
}

#pred trigram by default always run
for (x in seq_along(qn)) {
  ptm <- proc.time()
  pred <- predSimpleBackoff(qn[x], tf1, tf2, tf3)
  print(pred)
  print(proc.time()-ptm)
}