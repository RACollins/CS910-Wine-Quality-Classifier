library(sentimentr)
library(dplyr)
wineSA <- read_csv("wineTextAnalysis.csv")
wineSA_sampled <- read_csv("wineSA_sampled.csv")
wineSA$X1 <- NULL
wineSA_sampled$X1 <- NULL
#View(wineSA)

#test of sentimentr package
#review <- "This is ripe and fruity, a wine that is smooth while still structured. 
#  Firm tannins are filled out with juicy red berry fruits and freshened with acidity. 
#  It's already drinkable, although it will certainly be better from 2016."

#Function for extracting the sentiment from a review
Getting_Sentimental <- function(string){
  temp <- get_sentences(string)
  temp <- sentiment(temp)
  temp <- mean(temp[, sentiment])
  return(temp)
}

#Function to calculate the total words in a review
Get_Word_Count <- function(string){
  temp <- get_sentences(string)
  temp <- sentiment(temp)
  temp <- sum(temp[, word_count])
  return(temp)
}


#testing
#reviewSentiment <- Getting_Sentimental(review)
#reviewTotalWords <- Get_Word_Count(review)
#reviewSentiment
#reviewTotalWords

#wineSA_sampled <- wineSA[sample(nrow(wineSA), 13000), ]
#wineSA_sampled$reviewSentiment <- sapply(wineSA_sampled$description, FUN = Getting_Sentimental)
#wineSA_sampled$wordCount <- sapply(wineSA_sampled$description, FUN = Get_Word_Count)


