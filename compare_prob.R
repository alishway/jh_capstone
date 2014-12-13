require(ggplot2)
load("training/070/dictionaries/probC_700.RData")

p.uni <- prob.uni$prob@x
p.uni <- p.uni[p.uni>0]
p.bi <-prob.bi$prob@x
p.bi <- p.bi[p.bi>0]
p.tri <- prob.tri$prob@x
p.tri <- p.tri[p.tri>0]

hist.uni <- hist(p.uni, 0:256)
hist.bi <- hist(p.bi, 0:256)
hist.tri <- hist(p.tri, 0:256)

h.uni <- hist.uni$counts
h.bi <- hist.bi$counts
h.tri <- hist.tri$counts

d.uni <- hist.uni$density
d.bi <- hist.bi$density
d.tri <- hist.tri$density

hist.prob <- data.frame(b=rep(0:255, times=3), h=c(h.uni, h.bi, h.tri),
                        d=c(d.uni, d.bi, d.tri),
                        m=rep(c("Unigram", "Bigram", "Trigram"), each=256))

qplot(b, d, data=hist.prob, colour= m)
