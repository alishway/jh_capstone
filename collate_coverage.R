####
# load term document matrices to calculate its sorted term frequency
####

source('~/GitHub/jh_capstone/JHCap_lib.R')

outpath <- "rawdata"

x <- .002

input.idx <- c(seq(.002, .022, .002), seq(.03, .15, .01))
#input.idx <- seq(.002, .008, .002)

for (x in input.idx) {
  print(x)
  ptm <- proc.time()
  subdir <- file.path(outpath, sprintf("%03.0f", 1000 * x))
  load(file.path(subdir, "tdm_n1.RData"))
  print(proc.time()-ptm)
  
  system.time(tf <- countTermFreq(tdm))
  
  save(tf, file = file.path(subdir, "tf.RData"))
  print(proc.time()-ptm)
  print("done")
}