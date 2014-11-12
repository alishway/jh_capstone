# cleanup of corpus
##################

require(tm)

#chardata <- s.twits  # simulate passing parameter

CreateCorp <- function (chardata, tag) {
  corp <- VCorpus(VectorSource(chardata))

  #tag source of sentence
  corp <- tm_map(corp, function(x) {
    meta(x, "origin") <- tag
    x
  })
  return(corp)
}

#getting all hashtags
hashtag.pattern <- "#[0-9a-zA-Z]+"
ht <- regmatches(chardata, gregexpr(hashtag.pattern, chardata))
# TO-DO: embed as tag to the corpus

# remove all hashtags before passing to corpus
corp <- CreateCorp(gsub(hashtag.pattern, "", chardata), "twits")

corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, stemDocument)

tdm <- TermDocumentMatrix(corp)

# convert back to char
clean.char <- unlist(sapply(corp, "[", "content"))
names(clean.char) <- NULL

