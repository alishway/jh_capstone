#####
# create a combined file with variables of uni-/bi-/trigram term frequency
# of coverage x% of complete term frequency
####

source('~/GitHub/jh_capstone/JHCap_lib.R')

sourcepath <- "_fullcorp"
outpath <- "dictionaries/tf4"

reloadFlag =FALSE  # switch off to FALSE to save time if this has been run earlier

if (reloadFlag) {
  print("Reloading")
  print(system.time(load(file.path(sourcepath, "tf4_full.RData"))))
  print("tf4_full.RData reloaded")
  
  full.tf4 <- sort(full.tf4, decreasing=TRUE)
  #no removal of singleton as full.tf4 is already without singleton, see merge_tf4.R
  print("sorting done")
}


trimTF <- function (tf, coverage) {
# assume full.tf already sorted descending order for faster process
#  tf <- sort(full.tf, decreasing=TRUE)
  cf <- calcFreqCutoff(tf, x)
  return (tf[1:cf])
}

coverage <- c(seq(.5, .8, .1), seq(.9, .99, .01))  # 1 is 100%

for (x in coverage) {
  print(x)
  print(system.time(tf4 <- trimTF(full.tf4, x)))
  print("tf4 trimmed")
  print(system.time(save(tf4,
            file=file.path(outpath, sprintf("tf4_%03.0f.RData", x*1000)))))
}