source('~/GitHub/jh_capstone/JHCap_lib.R')

require(compiler)
enableJIT(3)  # to force compile even nested functions

####
# combine term frequency tables of smaller pieces of sample term frequencies
# for an n-gram
####
tfCombiner <- function(full.tf, ngram=1, outpath="training", n1=1, n2=100) {
  for (x in n1:n2) {
    print(x)
    ptm <- proc.time()  # start timer
    gc()
    subdir <- file.path(outpath, sprintf("%03.0f", x))
    load(file.path(subdir, "tf.RData"))
    print(proc.time()-ptm)
    
    # IMPORTANT NOTE: the tapply causes the sorted full.tf1 to be unsorted
    # re-sorting needed in subsequent phase (e.g. tf_coverage.R)
    
    if (ngram==1) {
      full.tf <- c(full.tf, tf1)
    } else if (ngram==2) {
      full.tf <- c(full.tf, tf2)
    } else if (ngram==3) {
      full.tf <- c(full.tf, tf3)  
    } else if (ngram==4) {
      full.tf <- c(full.tf, tf4)
    }
    
    full.tf <- tapply(full.tf, names(full.tf), sum)

    #temporary save
#    if (x %in% c(30, 60, 70, 80, 90, 95)) {
#      print("Save temp file")
#      save(full.tf, file = file.path(outpath, sprintf("tempfile_%03.0f_ngram%d", x, ngram)))
#      print(proc.time()-ptm)
#    }

    print(proc.time()-ptm)
    
    print("complete")

  }
  return(full.tf)
}

compTFCombiner <- cmpfun(tfCombiner)


####
# combine term frequency tables of smaller pieces of sample term frequencies
# for all n-grams
####
tfCombinerBuild <- function(path="training", train.pct=seq(.7, .9, .05)) {
  timing = TRUE
  ptm <- proc.time()  # start timer

  for (x in train.pct) {
    print(x)
    ptm <- proc.time()  # start timer
    #outpath <- "_fullcorp"
    full.tf1 <- numeric(0)
    full.tf2 <- numeric(0)
    
    full.tf4 <- numeric(0)
    
    outpath <- file.path(path, sprintf("%03.0f", x*100))
    
    #due to memory drain, do tf1, 2, 3, and 4 one at a time
    full.tf1 <- compTFCombiner(full.tf1, 1, outpath)
    if (timing) print(proc.time()-ptm)
    save(full.tf1, file = file.path(outpath, "tf1_full.RData"))
    rm(full.tf1)
    gc()
    if (timing) print(proc.time()-ptm)
    
    full.tf2 <- compTFCombiner(full.tf2, 2, outpath)
    if (timing) print(proc.time()-ptm)
    save(full.tf2, file = file.path(outpath, "tf2_full.RData"))
    rm(full.tf2)
    gc()
    if (timing) print(proc.time()-ptm)
    
    for (y in 1:4) {
      full.tf3 <- numeric(0)
      print(y)
      full.tf3 <- compTFCombiner(full.tf3, 3, outpath, y*25-24, y*25)
      if (timing) print(proc.time()-ptm)
      save(full.tf3, file = file.path(outpath, sprintf("tf3_full_%d.RData", y)))
      rm(full.tf3)
      gc()
      if (timing) print(proc.time()-ptm)
    }
    
    for (y in 1:5) {
      full.tf4 <- numeric(0)
      print(y)
      full.tf4 <- compTFCombiner(full.tf4, 4, outpath, y*20-19, y*20)
      if (timing) print(proc.time()-ptm)
      save(full.tf4, file = file.path(outpath, sprintf("tf4_full_%d.RData", y)))
      rm(full.tf4)
      gc()
      if (timing) print(proc.time()-ptm)
    }
    
    print("trained")
  }  
}

compTFCombinerBuild <- cmpfun(tfCombinerBuild)

#And Start$

#compTFCombinerBuild("training", seq(.7, .9, .05))
compTFCombinerBuild("training", .7)