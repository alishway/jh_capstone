#####
# remove bigrams and trigrams with cutoff using the frequency of 90% cutoff of unigram
####


load("rawdata/250/tf.RData")

#tf1 <- countTermFreq(tdm)
#tf2 <- countTermFreq(tdm2)
#tf3 <- countTermFreq(tdm3)

cf1 <- calcFreqCutoff(tf1)
cf2.1 <- max(which(tf2==tf1[cf1]))
cf3.1 <- max(which(tf3==tf1[cf1]))

c.tf1 <- tf1[1:cf1]
c.tf2 <- tf2[1:cf2.1]
c.tf3 <- tf3[1:cf3.1]