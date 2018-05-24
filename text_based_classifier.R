library(magrittr)
library(rockchalk)
library(caret)
library(dplyr)
#Add "highQuality" column and drop NA from "price" column
wineSA_sampled$highQuality <- wineSA_sampled$points >= 88
wineSA_sampled$highQuality <- as.integer(as.logical(wineSA_sampled$highQuality))

#Make sure no NA
wineSA_sampled <- wineSA_sampled[complete.cases(wineSA_sampled),]

#Create partition and define training and testing data sets
set.seed(3456)
trainIndex <- createDataPartition(wineSA_sampled$highQuality, p = .67,
                                  list = FALSE,
                                  times = 1)
Train <- wineSA_sampled[ trainIndex,]
Test  <- wineSA_sampled[-trainIndex,]

#Define logistic regression model
lfit <- glm(highQuality ~ wordCount + reviewSentiment, data = Train, family = binomial)
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

best_threshold <- thresholds[which.max(accuracy)]
#best_threshold <- 0.5

#Add probability and prediction columns
Test <- Test  %>% mutate(model_pred = 1*(model_prob > best_threshold) + 0)
Test <- Test %>% mutate(accurate = 1*(model_pred == highQuality))
model_accuracy <- sum(Test$accurate)/nrow(Test)

#Plot graph of the data space
data_space <- ggplot(data = wineSA_sampled, mapping = aes(x = wordCount, y = highQuality, color = points)) +
  geom_jitter(height = 0.4, width = 0.01, alpha = 0.1) +
  #xlim(-2, 2) +
  #ylim(-0.5, 1.5) +
  scale_color_gradient(low="blue", high="red")
#data_space

model_accuracy
table(Test$highQuality, Test$model_prob > best_threshold) #Confusion matrix
