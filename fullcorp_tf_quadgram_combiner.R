source('~/GitHub/jh_capstone/JHCap_lib.R')

outpath <- "_fullcorp"
full.tf4 <- numeric(0)

#due to memory drain, split into groups of 5
#to be merged later with merge_tf4.R

for (y in 0:4) {
  for (z in 1:20) {
    x <- y*20 + z
    print(x)
    ptm <- proc.time()  # start timer
    subdir <- file.path(outpath, sprintf("%03.0f", x))
    load(file.path(subdir, "tf4.RData"))
    print(proc.time()-ptm)
    
    # IMPORTANT NOTE: the tapply causes the sorted full.tf1 to be unsorted
    # re-sorting needed in subsequent phase (e.g. tf_coverage.R)
    
    
    full.tf4 <- c(full.tf4, tf4)
    full.tf4 <- tapply(full.tf4, names(full.tf4), sum)
    print(proc.time()-ptm)
    
    print("complete")
  }
  
  save(full.tf4, file = file.path(outpath, paste0("tf4_full_", y+1, ".RData"))  
}
