#####
# remove bigrams and trigrams with cutoff using the frequency of 90% cutoff of unigram
####

tf <- countTermFreq(tdm)
tf2 <- countTermFreq(tdm2)
tf3 <- countTermFreq(tdm3)

cf <- calcFreqCutoff(tf)
cf2.1 <- which(tf2==tf[cf])
cf3.1 <- which(tf3==tf[cf])