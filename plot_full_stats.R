###################
# plot statistics for full list of text
###################

require(ggplot2)

#load("rawdata/rawdata.RData")
load("rawdata/full/stats.RData")

#comp.stats <- rbind(blog.stats, news.stats, twit.stats)
#comp.stats$source <- c("Blog", "News", "Twitter")

c.blogs <- nchar(blogs)
c.news <- nchar(news)
c.twits <- nchar(twits)

par(mfrow=c(3,1))

hb <- hist(c.blogs, breaks=max(c.blogs), xlim=c(0,1000))

hn <- hist(c.news, breaks=max(c.news), xlim=c(0,1000))

ht <- hist(c.twits, breaks=max(c.twits), xlim=c(0,1000))

save(hb, hn, ht, file="rawdata/full/hist.RData")
