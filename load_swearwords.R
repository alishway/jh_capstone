#load and return swearwords, for filtering
swearWords <- function(source="swearWords.csv") {
  con <- file(source, "rb")
  swears <- readLines("swearWords.csv")
  close(con)
  return(unlist(strsplit(swears, ",")))
}