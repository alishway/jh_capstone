### library of functions for cleaning up text / corpus 
### for JH Capstone project on NLP

require(compiler)
enableJIT(3)  # to force compile even nested functions

require(tm)


####
# Clean up a corpus
####
cleanCorp <- function (corp) {
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removePunctuation)
#                 preserve_intra_word_dashes=TRUE)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, stripWhitespace)
  return(corp)
}

compCleanCorp <- cmpfun(cleanCorp)

####
# Clean up text before made into corpus
####
cleanText <- function(texts) {
  c.texts <- gsub(hashtag.pattern, "", texts)
#  c.texts <- gsub('â€“', '–', c.texts)
#  c.texts <- gsub('â€™', '’', c.texts)
#  c.texts <- gsub('â€œ', '“', c.texts)
#  c.texts <- gsub("â€[[:cntrl:]]", '”', c.texts)
  c.texts <- gsub('â€“', '', c.texts)
  c.texts <- gsub('â€™', '', c.texts)
  c.texts <- gsub('â€œ', '', c.texts)
  c.texts <- gsub("â€[[:cntrl:]]", '', c.texts)
  
  return(c.texts)
}

compCleanText <- cmpfun(cleanText)


####
# convenience function to prepare text into clean corpus
####
prepCorp <- function (texts) {
  clean.text <- compCleanText(texts)
  corp <- VCorpus(VectorSource(clean.text))
  corp <- compCleanCorp(corp)
  return(corp)
}

compPrepCorp <- cmpfun(prepCorp)


####
# convenience function to return clean corpus as its text
####
prepText <- function(fulltext) {
  texts <- sapply(compPrepCorp(fulltext), function(x) return(x$content))
  return(texts)
}

compPrepText <- cmpfun(prepText)