#file and path definitions
data.path <- "rawdata/"
f.twit <- paste0(data.path, "en_US.twitter.txt")
f.blog <- paste0(data.path, "en_US.blogs.txt")
f.news <- paste0(data.path, "en_US.news.txt")

#read data
twit.con <- file(f.twit, "rb")
twits <- readLines(twit.con)
blog.con <- file(f.blog, "rb")
blogs <- readLines(blog.con)
news.con <- file(f.news, "rb")
news <- readLines(news.con)


close(twit.con)
close(blog.con)
close(news.con)

save(twits, blogs, news, file="rawdata.RData", compress="xz")