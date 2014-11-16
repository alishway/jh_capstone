###################
# create database of corpuses with source tag
###################

load("rawdata/rawdata.RData")
text.src <- c(rep("Blog", length(blogs)), rep("News", length(news)), rep("Twitter", length(twits)))
full.corp <- data.frame(text=rbind(blogs, news, twits), source=text.src)

save(full.corp, file="rawdata/full_corpus_df.RData")