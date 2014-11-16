require(ggplot2)

#load("rawdata/full/stats.RData")
#system.time(rw <- unlist(strsplit(s, " ")))
#rough.words <- length(rw)
#rough.unique.words <- length(unique(rw))


#load("allstats.RData")

all.stats <- within(all.stats, {
  ave.rough.words <- rough.words/lines
  ave.terms <- terms/lines
  ratio.unique.terms <- unique.terms / terms
  ratio.unique.words <- rough.unique.words/ rough.words
  ratio.unique.bigrams <- unique.bigrams / bigrams
  ratio.unique.trigrams <- unique.bigrams / trigrams
})

