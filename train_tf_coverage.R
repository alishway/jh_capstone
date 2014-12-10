#####
# create a combined file with variables of uni-/bi-/trigram term frequency
# of coverage x% of complete term frequency
####

source('~/GitHub/jh_capstone/JHCap_lib.R')

sourcepath <- "training/070"
outpath <- "training/070/dictionaries"

reloadFlag =FALSE  # switch off to FALSE to save time if this has been run earlier

if (reloadFlag) {
  print("Reloading")
  print(system.time(load(file.path(sourcepath, "tf1_full.RData"))))
  print("tf1_full.RData reloaded")
  print(system.time(load(file.path(sourcepath, "tf2_full.RData"))))
  print("tf2_full.RData reloaded")
  print(system.time(load(file.path(sourcepath, "tf3_full.RData"))))
  print("tf3_full.RData reloaded")
  print(system.time(load(file.path(sourcepath, "tf4_full.RData"))))
  print("tf4_full.RData reloaded")
  
  full.tf1 <- sort(full.tf1, decreasing=TRUE)
  
  full.tf2 <- sort(full.tf2, decreasing=TRUE)
  #remove all singleton
  cf2 <- length(full.tf2) - sum(full.tf2 == 1)
  full.tf2 <- full.tf2[1:cf2]
  
  full.tf3 <- sort(full.tf3, decreasing=TRUE)
  cf3 <- length(full.tf3) - sum(full.tf3 == 1)
  full.tf3 <- full.tf3[1:cf3]
  
  full.tf4 <- sort(full.tf4, decreasing=TRUE)
  cf4 <- length(full.tf4) - sum(full.tf4 == 1)
  full.tf4 <- full.tf4[1:cf4]
  
  print("sorting done")
}


trimTF <- function (tf, coverage) {
# assume full.tf already sorted descending order for faster process
#  tf <- sort(full.tf, decreasing=TRUE)
  cf <- calcFreqCutoff(tf, x)
  return (tf[1:cf])
}

coverage <- c(seq(.6, .8, .1), seq(.9, .99, .01))  # 1 is 100%

for (x in coverage) {
  print(x)
  gc()
  print(system.time(tf1 <- trimTF(full.tf1, x)))
  print("tf1 trimmed")
  print(system.time(tf2 <- trimTF(full.tf2, x)))
  print("tf2 trimmed")
  print(system.time(tf3 <- trimTF(full.tf3, x)))
  print("tf3 trimmed")
  print(system.time(tf4 <- trimTF(full.tf4, x)))
  print("tf4 trimmed")
  print(system.time(save(tf1, tf2, tf3, tf4
            file=file.path(outpath, sprintf("tf_%03.0f.RData", x*1000)))))
}