library(stringr)
Clean_String <- function(string){
  # Lowercase
  temp <- tolower(string)
  #' Remove everything that is not a number or letter (may want to keep more 
  #' stuff in your actual analyses). 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- stringr::str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

Get_Length <- function(vector){
  length <- length(vector)
  return(length)
}

#test
#sentence <- "The term 'data science' (originally used interchangeably with 'datalogy') has existed for over thirty years and was used initially as a substitute for computer science by Peter Naur in 1960."
#clean_sentence <- Clean_String(sentence)

wineTA$words <- lapply(wineTA$description, Clean_String)

wineTA$num_words <- lapply(wineTA$words, Get_Length)
wineTA$num_words <- as.numeric(wineTA$num_words)
wineTA_sampled <- wineTA[sample(nrow(wineTA), 30000), ]

WVPPlot <- ggplot(data = wineTA, mapping = aes(x = num_words, y = points)) +
  geom_jitter(height = 0.5, width = 0.01, alpha = 0.1)
WVPPlot



