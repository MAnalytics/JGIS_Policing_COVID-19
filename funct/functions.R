
#function to put PC to sleep for an x number of seconds
testit <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

#data cleaning functions

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("\"", " ", doc)
  doc <- gsub("\n", " ", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
