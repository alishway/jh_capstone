source('~/GitHub/jh_capstone/JHCap_lib.R')

outpath <- "_fullcorp"
full.tf1 <- numeric(0)
full.tf2 <- numeric(0)
full.tf3 <- numeric(0)

#due to memory drain, suggest to do tf1, 2, or 3 one at a time

for (x in 76:100) {
  print(x)
  ptm <- proc.time()  # start timer
  subdir <- file.path(outpath, sprintf("%03.0f", x))
  load(file.path(subdir, "tf.RData"))
  print(proc.time()-ptm)

#  full.tf1 <- c(full.tf1, tf1)
#  full.tf1 <- tapply(full.tf1, names(full.tf1), sum)
#  print(proc.time()-ptm)
  
#  full.tf2 <- c(full.tf2, tf2)
#  full.tf2 <- tapply(full.tf2, names(full.tf2), sum)
#  print(proc.time()-ptm)
  
  full.tf3 <- c(full.tf3, tf3)
  full.tf3 <- tapply(full.tf3, names(full.tf3), sum)
  print(proc.time()-ptm)
  
  print("complete")
}

save(full.tf3, file = file.path(outpath, "tf3_full_4.RData"))