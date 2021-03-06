library(magrittr)
library(rockchalk)
library(caret)
library(dplyr)
#Add "highQuality" column and drop NA from "price" column
wineData$highQuality <- wineData$points >= 88
wineData$highQuality <- as.integer(as.logical(wineData$highQuality))
wineDataDropNAPrice <- wineData[complete.cases(wineData$price),]

#Create partition and define training and testing data sets
set.seed(3456)
trainIndex <- createDataPartition(wineDataDropNAPrice$highQuality, p = .67,
                                  list = FALSE,
                                  times = 1)
Train <- wineDataDropNAPrice[ trainIndex,]
Test  <- wineDataDropNAPrice[-trainIndex,]


#Define logistic regression model
lfit <- glm(highQuality ~ price, data = Train, family = binomial)
summary(lfit)

#use the test set to calulate the model accuracy
Test$model_prob <- predict(lfit, Test, type = "response")

#Find the threshold that gives the highest accuracy
thresholds <- seq(0.01, 1, 0.01)
accuracy <- NULL
for (i in seq(along = thresholds)){
  prediction <- ifelse(Test$model_prob > thresholds[i], 1, 0) #Predicting for threshold
  accuracy <- c(accuracy,length(which(Test$highQuality == prediction))/length(prediction))
}
#best_threshold <- thresholds[which.max(accuracy)]
best_threshold <- 0.50

#Add probability and prediction columns
Test <- Test  %>% mutate(model_pred = 1*(model_prob > best_threshold) + 0)
Test <- Test %>% mutate(accurate = 1*(model_pred == highQuality))
model_accuracy <- sum(Test$accurate)/nrow(Test)

model_accuracy
table(Test$highQuality, Test$model_prob > best_threshold) #Confusion matrix

#Plot graph of the data space
data_space <- ggplot(data = wineDataDropNAPrice, mapping = aes(x = price, y = highQuality)) +
  geom_jitter(height = 0.2,  width = 0.1, alpha = 0.1) +
  labs(x = "price ($)", y = "probability that wine is high quality") +
  scale_x_continuous(trans = 'log10') +
  geom_line(data = Test, mapping = aes(x = price, y = model_prob), color = "dodgerblue")
#data_space
