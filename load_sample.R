outpath <- "rawdata"
x <- .008
subdir <- file.path(outpath, sprintf("%03.0f", 1000*x))

load(file.path(subdir, "rawchar.RData"))
load(file.path(subdir, "cleaned_corpus.RData"))
load(file.path(subdir, "tdm_n1.RData"))
load(file.path(subdir, "stats.RData"))

