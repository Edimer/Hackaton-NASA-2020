# Function clean text
cleanText <- function(text){
  
  # Load library
  suppressMessages(suppressWarnings(library(tm)))
  
  # Tokenize text
  text = gsub("[[:cntrl:]]", " ", text)
  text = tolower(text)
  text = removePunctuation(text)
  text = removeNumbers(text)
  text = stripWhitespace(text)
  text = removeWords(text, words = tm::stopwords(kind = "SMART"))
  text = str_split(text, " ")[[1]]  
  text = keep(.x = text, .p = function(x){str_length(x) > 2})
  
  # Return
  return(text)
}