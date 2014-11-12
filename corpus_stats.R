# create statistics of corpuses
###################

subsample <- function(rawdata, percent) {
  len <- length(rawdata)
  idx <- rbinom(percent*len, len, prob=.5 )
  return(rawdata[idx])
}



#load("rawdata.RData")

# if no variables "blogs", "news", and "twits",
# source('~/GitHub/jh_capstone/load_corpus.R')  to read into the above

twits.len <- nchar(twits)
blogs.len <- nchar(blogs)
news.len <- nchar(news)

par(mfcol=c(3,1))
hist(twits.len, breaks=50)
hist(blogs.len, breaks=50)
hist(news.len, breaks=50)

rec.count <- length(blogs)+length(news)+length(twits)

sample.pct <- .001
s.twits <- subsample(twits, sample.pct)
s.blogs <- subsample(blogs, sample.pct)
s.news <- subsample(news, sample.pct)

save(s.twits, s.blogs, s.news, file="sampled data.RData")
