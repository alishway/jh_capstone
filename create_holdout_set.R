source('~/GitHub/jh_capstone/JHCap_lib.R')

setpath <- "training/070"

generate.holdout <- TRUE  #generate holdout and test set, or reuse
generate.targets <- TRUE  #extract target words from final test set, or load

if (generate.holdout) {
  load(file.path(setpath, "dataset.RData"))
  
  #clean up trailing and leading spaces
  test.set <- gsub("^ +", "", test.set)
  test.set <- gsub(" +$", "", test.set)
  
  #remove empty and single-word texts
  word.count <- sapply(strsplit(test.set, " "), length)
  test.set <- test.set[word.count>1]
  
  #set aside holdout
  set.seed(19790501)
  
  holdout.idx <- rbinom(length(test.set), 1, .5)
  holdout.set <- test.set[holdout.idx==1]
  final.test.set <- test.set[holdout.idx==0]
  
  save(holdout.set, file=file.path(setpath, "holdout_set.RData"))
  save(final.test.set, file=file.path(setpath, "final_test_set.RData"))
} else {
  load(file.path(setpath, "holdout_set.RData"))
}

#####
# randomly choose a word as target for prediction in a list of text
#####
generateTargets <- function(texts, seed=1705) {
  words.ft <- strsplit(texts, " ")
  word.count <- sapply(words.ft, length)
  
  #choose the words to predict (must be at least 2nd word)
  set.seed(1705)
  pred.idx <- sapply(as.list(word.count), function (x) {sample(2:x, 1)})
  # get all words to predict i.e. answers for training
  target <- mapply("[", words.ft, pred.idx)
  # get all phrase preceding answers above, i.e. phrase to predict the word
  phrase <- mapply(function (x,y)
    {do.call(paste, c(as.list(x[1:(y-1)]), collapse=""))}, words.ft, pred.idx)
  return(list(phrase=phrase, target=target))
}

if (generate.targets) {
  ptm <- proc.time()
  print("final set")
  target.final <- generateTargets(final.test.set)
  print(proc.time()-ptm)
  
  print("holdout set")
  target.holdout <- generateTargets(holdout.set)
  print(proc.time()-ptm)
  
  print("saving")
  save(target.final, file=file.path(setpath, "target_words_test.RData"))
  save(target.holdout, file=file.path(setpath, "target_words_holdout.RData"))
  print(proc.time()-ptm)
  
  print("Completed")
} else {
  load(file.path(setpath, "target_words.RData"))
}

