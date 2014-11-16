####
# load term document matrices to calculate its sorted term frequency
####

outpath <- "rawdata"

all.stats <- data.frame(sample.pct=numeric(),
                        lines=numeric(), chars=numeric(),
                        ave.char=numeric(), var.char=numeric(),
                        terms=numeric(), unique.terms=numeric(),
                        rough.words=numeric(), rough.unique.words=numeric(),
                        bigrams=numeric(), unique.bigrams=numeric(),
                        trigrams=numeric(), unique.trigrams=numeric()
)
x <- .002

#input.idx <- c(seq(.002, .022, .002), seq(.03, .15, .01))
input.idx <- seq(.002, .008, .002)

for (x in input.idx) {
  print(x)
  ptm <- proc.time()
  subdir <- file.path(outpath, sprintf("%03.0f", 1000 * x))
  load(file.path(subdir, "stats.RData"))  
  load(file.path(subdir, "rawchar.RData"))
  #load(file.path(subdir, "cleaned_corpus.RData"))
  load(file.path(subdir, "tdm_n1.RData"))
  terms <- sum(tdm$v)
  unique.terms <- tdm$nrow
  print(proc.time()-ptm)
  
  load(file.path(subdir, "tdm_n2.RData"))
  bigrams <- sum(tdm2$v)
  unique.bigrams <- tdm2$nrow
  rm(tdm2)
  print(proc.time()-ptm)
  
  load(file.path(subdir, "tdm_n3.RData"))
  trigrams <- sum(tdm3$v)
  unique.trigrams <- tdm3$nrow
  rm(tdm3)
  print(proc.time()-ptm)
  
  sample.pct <- x
  
  rw <- unlist(strsplit(s, " "))
  rough.words <- length(rw)
  rough.unique.words <- length(unique(rw))
  all.stats <- rbind(all.stats, 
                     cbind(sample.pct, corp.stats, terms, unique.terms,
                           rough.words, rough.unique.words,
                           bigrams, unique.bigrams, trigrams, unique.trigrams))
  print(proc.time()-ptm)
  print("done")
}


save(all.stats, file="allstats.RData")