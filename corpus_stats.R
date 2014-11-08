# create statistics of corpuses
###################

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