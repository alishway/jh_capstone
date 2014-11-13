# create subsample of corpuses
###################

subsample <- function(rawdata, percent) {
  len <- length(rawdata)
  idx <- rbinom(percent*len, len, prob=.5 )
  return(rawdata[idx])
}


#load("rawdata.RData")

# if no variables "blogs", "news", and "twits",
# source('~/GitHub/jh_capstone/load_corpus.R')  to read into the above


sample.pct <- .001
s.twits <- subsample(twits, sample.pct)
s.blogs <- subsample(blogs, sample.pct)
s.news <- subsample(news, sample.pct)

save(s.twits, s.blogs, s.news, file="sampled data.RData")
