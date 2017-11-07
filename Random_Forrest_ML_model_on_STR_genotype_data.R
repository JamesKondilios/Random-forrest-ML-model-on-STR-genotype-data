setwd("/Users/studentadmin/Desktop/Folders/BIOL2151_POPULATION_CLASSIFICATION/Data")

library(caret)
library(ranger)

# Number of repeats with randomly selected training and test data sets
repeats <- 1

# rawwwww data
STR_data <- read.csv("ANU_STR_DAT.csv", colClasses=c( rep('factor', 31))) 

# accuracy vectors:
accuracy.all <- as.numeric()

#custom tune grid
mygrid <- data.frame(mtry = c(10:30))

#model trained on all 3 populations
#model tested on all 3 populations - test set includes all populations

#start time
t1 <- timestamp()

# Loop to test accuracy of  model x times
for(x in 1:repeats){
  
  # Random partitioning of test and training data set
  rows <- sample(nrow(STR_data))
  STR_data <- STR_data[rows,]
  split <- round(nrow(STR_data) * 0.6)
  train <- STR_data[1:split, ]
  test <-  STR_data[(split + 1):nrow(STR_data), ]
  
  # Applying model on training dataset
  model <- train(Pop ~ ., tuneGrid = mygrid, data = `train`, method = "ranger", trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
  
  # Making predictions on test dataset
  pred <- predict(object = model, test)
  
  # Generation of a confusion matrix and accuracy statistics. The confusionMatrix function generates these statistics automatically for us.
  cm  <- confusionMatrix(pred, test[["Pop"]])
  
  accuracy.all  <- append(accuracy.all ,cm$overall[["Accuracy"]])
  
  # removing un-needed objects from the global name space: marginally increase speed and reduce overall RAM needed
  rm(`split`, train, test, rows, pred, model)
  
  
}

#end time
t2 <- timestamp()


# changing accuracy.all from a numeric to a data.frame
accuracy.all <- data.frame(accuracy.all)

# Accuracy statistics are contained within the 'accuracy.all' dataframe.
print(mean(accuracy.all$accuracy))





