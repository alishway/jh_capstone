# script to generate training dataset

source('~/GitHub/jh_capstone/fullcorp_tf_generator_lib.R')

timing <- TRUE
clean.text <- TRUE
load.text <- FALSE

outpath <- "training"
#train.pct <- seq(.85, .9, .05)
#train.pct <- .7
train.pct <- numeric(0)

if (clean.text) {
  ptm <- proc.time()
#  load("rawdata/rawdata.RData")
  if (timing) print(proc.time()-ptm)
  
  # clean off hashtags, etc
  c.twits <- compPrepText(twits)
  if (timing) print(proc.time()-ptm)
  
  c.blogs <- compPrepText(blogs)
  if (timing) print(proc.time()-ptm)
  
  c.news <- compPrepText(news)
  if (timing) print(proc.time()-ptm)

  save(c.twits, c.blogs, c.news, file=file.path(outpath, "cleaned_texts_full.RData"))
}

if (load.text) {
  ptm <- proc.time()  # start timer
  load(file.path(outpath, "cleaned_texts_full.RData"))
  if (timing) print(proc.time()-ptm)
}

for (x in train.pct) {
  print(x)
  ptm <- proc.time()  # start timer

  subdir <- file.path(outpath, sprintf("%03.0f", 100*x))
  dir.create(subdir, showWarnings = FALSE)

  i.twits <- subsample.idx(c.twits, x)
  i.blogs <- subsample.idx(c.blogs, x)
  i.news <- subsample.idx(c.news, x)
  if (timing) print(proc.time()-ptm)
  
  train.set <- c(c.twits[i.twits], c.blogs[i.blogs], c.news[i.news])
  test.set <- c(c.twits[-i.twits], c.blogs[-i.blogs], c.news[-i.news])
  save(train.set, test.set, file=file.path(subdir,"dataset.RData"))
  rm(test.set)
  rm(i.twits, i.blogs, i.news)
  if (timing) print(proc.time()-ptm)
  
  compTFGenerator(train.set, subdir)
  
}
