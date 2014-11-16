####
# load sorted term frequency, plot 90% coverage
####

require(ggplot2)
require(scales)

#input.idx <- .002
#input.idx <- c(seq(.002, .022, .002), seq(.03, .15, .01))
#input.idx <- seq(.002, .008, .002)
input.idx <- c(.002, .01, .1)
c.idx <- length(input.idx)

cntf.df <- data.frame(word = numeric(), freq = numeric(), pct = factor())
x50 <- numeric(0)
x90 <- numeric(0)

for (x in input.idx) {
  subdir <- (file.path("rawdata", sprintf("%03.0f", 1000 * x)))

  load(file.path(subdir, "tf.RData"))

  cn.tf <- cumsum(tf/sum(tf))
  len <- length(cn.tf)
  #get indexes for the x-intercept of word covering the 90% of frequency
    x90 <- c(x90, length(which(cn.tf < .9))+1 + nrow(cntf.df))
  cntf.df <- rbind(cntf.df, data.frame(word=seq(1:len)/len,
                                       freq=cn.tf, pct=as.factor(rep(x, len))))
}

xint.90 <- cntf.df$word[x90]
pts <- data.frame(x=xint.90, label=sprintf("%2.2f %%", xint.90*100),
                  pct=as.factor(input.idx))

p <- ggplot(data=cntf.df, aes(x=word, y=freq, group=pct, colour=pct)) +
  ggtitle("Coverage by unique words") +
  geom_line() +
  scale_y_continuous(breaks=seq(0, 1, .1), labels=percent_format(),
                     name="% of all words in sample") +
  scale_x_continuous(breaks=seq(0, 1, .2), labels=percent_format(),
                     name="% of unique words in sample") +
  scale_colour_discrete(name="Sample size (% of total)",
                        labels=sprintf("%2.2f %%", input.idx*100)) +
  geom_point(data=pts, aes(x=x, y=.9)) +
  geom_text(data=pts, aes(x=x, y=.89, label=label), hjust=-.1, size=2)

print(p)

