outpath <- "rawdata"

all.stats <- data.frame(sample.pct=numeric(),
                       lines=numeric(), chars=numeric(),
                       ave.char=numeric(), var.char=numeric()
                        )
x <- .002

for (x in seq(.002, .006, .002)) {
  subdir <- file.path(outpath, sprintf("%03.0f", 1000 * x))
  
  #load(file.path(subdir, "rawchar.RData"))
  #load(file.path(subdir, "cleaned_corpus.RData"))
  #load(file.path(subdir, "tdm_n1.RData"))
  load(file.path(subdir, "stats.RData"))
  sample.pct=x
  all.stats <- rbind(all.stats, cbind(sample.pct, corp.stats))
}
  
