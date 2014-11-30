source('~/GitHub/jh_capstone/JHCap_lib.R')

require(compiler)
enableJIT(3)  # to force compile even nested functions

####
# generate broken down files of corpora (to get around memory limitation size)
# for recombining in building term frequency table
# with full being the set of char (training or full) to generate tf
####
tfGenerator <- function(fulltext, folder="training") {
  timing = TRUE

  cu <- cut(1:length(fulltext), 100, labels=FALSE)

        
  for (x in (1:length(unique(cu)))) {
    print(x)
    ptm <- proc.time()  # start timer

    subdir <- file.path(folder, sprintf("%03.0f", x))
    dir.create(subdir, showWarnings = FALSE)
    corp <- VCorpus(VectorSource(fulltext[cu==x]))
    if (timing) print(proc.time()-ptm)
    
    tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(1,Inf)))
    save(tdm, file = file.path(subdir, "tdm_n1.RData"))
    if (timing) print(proc.time()-ptm)
    
    tf1 <- countTermFreq(tdm)
    rm(tdm)  # immediate remove to save memory
    if (timing) print(proc.time()-ptm)
    
    tdm2 <- TermDocumentMatrix(corp, control = list(tokenize = BigramTokenizer))
    save(tdm2, file = file.path(subdir, "tdm_n2.RData"))
    if (timing) print(proc.time()-ptm)
    
    tf2 <- countTermFreq(tdm2)
    rm(tdm2)  # immediate remove to save memory
    if (timing) print(proc.time()-ptm)
    
    tdm3 <- TermDocumentMatrix(corp, control = list(tokenize = TrigramTokenizer))
    save(tdm3, file = file.path(subdir, "tdm_n3.RData"))
    if (timing) print(proc.time()-ptm)
    
    tf3 <- countTermFreq(tdm3)
    rm(tdm3)  # immediate remove to save memory
    if (timing) print(proc.time()-ptm)

    tdm4 <- TermDocumentMatrix(corp, control = list(tokenize = QuadgramTokenizer))
    save(tdm4, file = file.path(subdir, "tdm_n4.RData"))
    if (timing) print(proc.time()-ptm)
    
    tf4 <- countTermFreq(tdm4)
    rm(tdm4)  # immediate remove to save memory
    if (timing) print(proc.time()-ptm)
    
    
    save(tf1, tf2, tf3, tf4, file = file.path(subdir, "tf.RData"))
    
    if (timing) print(proc.time()-ptm)
    print("complete")

  } 
  return(TRUE)
}

compTFGenerator <- cmpfun(tfGenerator)