####
# convert indices of probability terms from words to index of vocab
####


load("training/070/probC_700.RData")
bi.vocab <- strsplit(prob.bi$terms, " ")
tri.vocab <- strsplit(prob.tri$terms, " ")
vocab <- sort(unique(c(prob.uni$terms, unlist(bi.vocab), unlist(tri.vocab))))

p.uni <- sapply(prob.uni$terms, index)
names(p.uni) <- NULL
pr.uni <- prob.uni
pr.uni$terms <- p.uni
pr.uni$prob <- pr.uni$prob/255

cat("bigram")
p.bi <- do.call(rbind, lapply(bi.vocab, decompose))
pr.bi <- prob.bi
pr.bi$terms <- p.bi[, 2:3]  # first element is always 0; dropped to save space
pr.bi$prob <- pr.bi$prob/255

cat("trigram")
p.tri <- do.call(rbind, lapply(tri.vocab, decompose))
pr.tri <- prob.tri
pr.tri$terms <- p.tri
pr.tri$prob <- pr.tri$prob/255

save(vocab, pr.uni, pr.bi, pr.tri,
     file="training/070/probidxC_700.RData")