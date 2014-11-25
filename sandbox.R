doc <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
        "Hey sunshine, can you follow me and make me the",
#        "Very early observations on the Bills game: Offense still struggling but the",
#        "Go on a romantic date at the",
#        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
#        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
#        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
#        "Be grateful for the good times and keep the faith during the",
        "Ricky adds that you can you follow me and make hay",
        "If this isn't the cutest thing you've ever seen, then you must be")

source('~/GitHub/jh_capstone/JHCap_lib.R')

# Create a corpus from the text listed below
corp = VCorpus(VectorSource(doc))

# Run the transformation function using the pattern above
c.corp <- cleanCorp(corp)


tdm1 <- TermDocumentMatrix(c.corp, control=list(wordLengths=c(1,Inf)))
tdm2 <- TermDocumentMatrix(c.corp, control = list(tokenize = BigramTokenizer))
tdm3 <- TermDocumentMatrix(c.corp, control = list(tokenize = TrigramTokenizer))

tf1 <- countTermFreq(tdm1)
tf2 <- countTermFreq(tdm2)
tf3 <- countTermFreq(tdm3)

n.tf2 <- names(tf2)
n.tf1 <- sort(unique(unlist(strsplit(n.tf2, " "))))

n.tf3 <- names(tf3)
c.tf1 <- matrix(nrow=length(tf1), ncol=length(tf1))
c.tf2 <- matrix(nrow=length(tf2), ncol=length(tf2))

map1.tf <- sapply(n.tf2, coordTF, n.tf1)
#c.tf1[cbind(map1.tf[1, ], map1.tf[2, ])] <- tf2
#create as sparsematrix instead
c.tf1 <- sparseMatrix(map1.tf[1, ], map1.tf[2, ], x=tf2)
p.tf1 <- c.tf1/rowSums(c.tf1)

tst.tf1 <- ngramSeqCount(tf2)

map2.tf <- sapply(n.tf3, coordTF, n.tf2)
c.tf2 <- sparseMatrix(map2.tf[1, ], map2.tf[2, ], x=tf3)

tst.tf2 <- ngramSeqCount(tf3)

t.tf2 <- tst.tf2$terms
tc.tf2 <- tst.tf2$counts
