require(ggplot2)

#load("rawdata/full/stats.RData")
#system.time(rw <- unlist(strsplit(s, " ")))
#rough.words <- length(rw)
#rough.unique.words <- length(unique(rw))


load("allstats.RData")

all.stats <- within(all.stats, {
  ave.rough.words <- rough.words/lines
  ave.terms <- terms/lines
  ratio.unique.terms <- unique.terms / terms
  ratio.unique.words <- rough.unique.words/ rough.words
  ratio.unique.bigrams <- unique.bigrams / bigrams
  ratio.unique.trigrams <- unique.bigrams / trigrams
})

#p <- ggplot(data=all.stats, aes(x=sample.pct, )) +
#  ggline()

par(mfrow=c(1,3))
with(all.stats, {
  plot(sample.pct, ratio.unique.terms, ylim=c(0, .65))
  plot(sample.pct, ratio.unique.bigrams, ylim=c(0, .65))
  plot(sample.pct, ratio.unique.trigrams, ylim=c(0, .65))
})


with(all.stats, {
  plot(sample.pct, unique.terms, ylim=c(0, 10^7))
  plot(sample.pct, unique.bigrams, ylim=c(0, 10^7))
  plot(sample.pct, unique.trigrams, ylim=c(0, 10^7))
})