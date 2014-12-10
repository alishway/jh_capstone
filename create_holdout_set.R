source('~/GitHub/jh_capstone/JHCap_lib.R')

setpath <- "training/070"

generate.holdout <- TRUE  #generate holdout and test set, or reuse
generate.targets <- TRUE  #extract target words from holdout, or load

if generate.fl {
  load(file.path(setpath, "dataset.RData"))
  
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


if generate.targets {
  words.ho <- strsplit(holdout.set, " ")
  word.count <- sapply(words.ho, length)
  
  #choose the words to predict (must be at least 2nd word)
  set.seed(1705)
  pred.idx <- sapply(as.list(word.count), function (x) {sample(2:x, 1)})
  # get all words to predict i.e. answers for training
  target <- mapply("[", words.ho, pred.idx)
  # get all phrase preceding answers above, i.e. phrase to predict the word
  phrase <- mapply(function (x,y)
  {do.call(paste, c(as.list(x[1:(y-1)]), collapse=""))}, words.ho, pred.idx)
  
  save(target, phrase, file.path(setpath, "target_words.RData"))
} else {
  load(file.path(setpath, "target_words.RData"))
}

