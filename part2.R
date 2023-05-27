library(readxl)
library(Metrics)
library(neuralnet)

# Import data
uow_load <- read_excel("D:/Old/dinuwara/iit/2nd Year/2nd sem/MachineLearning/course work/ML/MachineLearningCW/uow_consumption.xlsx")

colnames(uow_load) <- c("Dates","18","19","20")

# convert the data into training and testing sets 
uow_load$Dates <- as.Date(uow_load$Dates)

# Normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Denormalizing
unnormalize <- function(x,min,max){
  return((max-min)*x + min)
}

# normalize data without Date column
normalizedUowData <- as.data.frame(lapply(uow_load[,-1],normalize))

# Split the data into training and testing sets
trainData <- normalizedUowData[1:380,]
testData <- normalizedUowData[381:470,]


##################################################################
####                        T-4 (5)
##################################################################

# Train data matrix
train_data_input <- matrix(,nrow = 0,ncol = 4)
colnames(train_data_input) <- c("t1","t2","t3","t4")
train_data_output <- c() # store output values


for(i in 1:length(trainData$X20)){
  day_counter <- i+4 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_input <- rbind(train_data_input,c(newInput)) 
  train_data_output <- append(train_data_output,newOutput)
}

# create new dataframe with new inputs and new outputs
newDataset <- cbind(as.data.frame(train_data_input),train_data_output)

# Test data matrix
test_data_input <- matrix(,nrow = 0,ncol = 4)
colnames(test_data_input) <- c("t1","t2","t3","t4")
test_data_output <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+4 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_input <- rbind(test_data_input,c(newInput)) 
  test_data_output <- append(test_data_output,newOutput)
}

# create test data frame
newDatasetForTest <- as.data.frame(test_data_input)

#min - max
minOfDataSet <- min(uow_load$`20`)
maxOfDataset <- max(uow_load$`20`)

# training module
modelForLinear4 <- neuralnet(train_data_output ~ t1 + t2 + t3 + t4, data=newDataset,hidden = 5, linear.output = TRUE)

plot(modelForLinear4)


# Making Prediction
resultsT4 <- compute(modelForLinear4,newDatasetForTest)

# result of neural network
result <- data.frame(actual = test_data_output,prediction = resultsT4$net.result)

# calculate accuracy
predict = result$prediction * abs(diff(range(test_data_output))) + min(test_data_output)
actual = result$actual * abs(diff(range(test_data_output))) + min(test_data_output)

comparison <- data.frame(predict,actual)
unnormalizeResults <- unnormalize(comparison,minOfDataSet,maxOfDataset)

deviation <- (unnormalizeResults$actual - unnormalizeResults$predict) / unnormalizeResults$actual

comparison <- data.frame(unnormalizeResults$predict,unnormalizeResults$actual,deviation)
accuracy = 1 - abs(mean(deviation))
accuracy

# get RMSE
RMSE_value <- rmse(unnormalizeResults$actual,unnormalizeResults$predict)
RMSE_value

# get MAE 
MAE_value <- mae(unnormalizeResults$actual,unnormalizeResults$predict)
MAE_value

# get MAPE
MAPE_value <- mape(unnormalizeResults$actual, unnormalizeResults$predict)
MAPE_value

# get sMAPE
sMape_value <- smape(unnormalizeResults$actual, unnormalizeResults$predict)
sMape_value




##################################################################
####                    T-4 (2 hidden layer)
##################################################################

# training module
modelForLinearT42 <- neuralnet(train_data_output ~ t1 + t2 + t3 + t4, data=newDataset,hidden = c(5,2), linear.output = TRUE)

plot(modelForLinearT42)

# Making Prediction
resultsT42 <- compute(modelForLinearT42,newDatasetForTest)

# result of neural network
resultT42 <- data.frame(actual = test_data_output,prediction = resultsT42$net.result)


# calculate accuracy
predictT42 = resultT42$prediction * abs(diff(range(test_data_output))) + min(test_data_output)
actualT42 = resultT42$actual * abs(diff(range(test_data_output))) + min(test_data_output)

comparisonT42 <- data.frame(predictT42,actualT42)
unnormalizeResultsT42 <- unnormalize(comparisonT42,minOfDataSet,maxOfDataset)

deviationT42 <- (unnormalizeResultsT42$actualT42 - unnormalizeResultsT42$predictT42) / unnormalizeResultsT42$actualT42

comparisonT42 <- data.frame(unnormalizeResultsT42$predictT42,unnormalizeResultsT42$actualT42,deviationT42)
accuracyT42 = 1 - abs(mean(deviationT42))
accuracyT42

# get RMSE
RMSE_valueT42 <- rmse(unnormalizeResultsT42$actualT42,unnormalizeResultsT42$predictT42)
RMSE_valueT42

# get MAE 
MAE_valueT42 <- mae(unnormalizeResults$actual,unnormalizeResults$predict)
MAE_valueT42

# get MAPE
MAPE_valueT42 <- mape(unnormalizeResults$actual, unnormalizeResults$predict)
MAPE_valueT42

# get sMAPE
sMape_valueT42 <- smape(unnormalizeResults$actual, unnormalizeResults$predict)
sMape_valueT42

##################################################################
####                        T-4 (10)
##################################################################

# training module
modelForLinearT43 <- neuralnet(train_data_output ~ t1 + t2 + t3 + t4, data=newDataset,hidden = 10, linear.output = TRUE)

plot(modelForLinearT43)


# Making Prediction
resultsT43 <- compute(modelForLinearT43,newDatasetForTest)

# result of neural network
resultT43 <- data.frame(actual = test_data_output,prediction = resultsT43$net.result)


# calculate accuracy
predictT43 = resultT43$prediction * abs(diff(range(test_data_output))) + min(test_data_output)
actualT43 = resultT43$actual * abs(diff(range(test_data_output))) + min(test_data_output)

comparisonT43 <- data.frame(predictT43,actualT43)
unnormalizeResultsT43 <- unnormalize(comparisonT43,minOfDataSet,maxOfDataset)

deviationT43 <- (unnormalizeResultsT43$actualT43 - unnormalizeResultsT43$predictT43) / unnormalizeResultsT43$actualT43

comparisonT43 <- data.frame(unnormalizeResultsT43$predictT43,unnormalizeResultsT43$actualT43,deviationT43)
accuracyT43 = 1 - abs(mean(deviationT43))
accuracyT43

# get RMSE
RMSE_valueT43 <- rmse(unnormalizeResultsT43$actualT43,unnormalizeResultsT43$predictT43)
RMSE_valueT43

# get MAE 
MAE_valueT43 <- mae(unnormalizeResultsT43$actualT43,unnormalizeResultsT43$predictT43)
MAE_valueT43

# get MAPE
MAPE_valueT43 <- mape(unnormalizeResultsT43$actualT43, unnormalizeResultsT43$predictT43)
MAPE_valueT43

# get sMAPE
sMape_valueT43 <- smape(unnormalizeResultsT43$actualT43, unnormalizeResultsT43$predictT43)
sMape_valueT43

##################################################################
####                    T-4 (2 hidden layer) (10,7)
##################################################################

# training module
modelForLinearT44 <- neuralnet(train_data_output ~ t1 + t2 + t3 + t4, data=newDataset,hidden = c(10,7), linear.output = TRUE)

plot(modelForLinearT44)


# Making Prediction
resultsT44 <- compute(modelForLinearT44,newDatasetForTest)

# result of neural network
resultT44 <- data.frame(actual = test_data_output,prediction = resultsT44$net.result)

# calculate accuracy
predictT44 = resultT44$prediction * abs(diff(range(test_data_output))) + min(test_data_output)
actualT44 = resultT44$actual * abs(diff(range(test_data_output))) + min(test_data_output)

comparisonT44 <- data.frame(predictT44,actualT44)
unnormalizeResultsT44 <- unnormalize(comparisonT44,minOfDataSet,maxOfDataset)

deviationT44 <- (unnormalizeResultsT44$actualT44 - unnormalizeResultsT44$predictT44) / unnormalizeResultsT44$actualT44

comparisonT44 <- data.frame(unnormalizeResultsT44$predictT44,unnormalizeResultsT44$actualT44,deviationT44)
accuracyT44 = 1 - abs(mean(deviationT44))
accuracyT44

# get RMSE
RMSE_valueT44 <- rmse(unnormalizeResultsT44$actualT44,unnormalizeResultsT44$predictT44)
RMSE_valueT44

# get MAE 
MAE_valueT44 <- mae(unnormalizeResultsT44$actualT44,unnormalizeResultsT44$predictT44)
MAE_valueT44

# get MAPE
MAPE_valueT44 <- mape(unnormalizeResultsT44$actualT44, unnormalizeResultsT44$predictT44)
MAPE_valueT44

# get sMAPE
sMape_valueT44 <- smape(unnormalizeResultsT44$actualT44, unnormalizeResultsT44$predictT44)
sMape_valueT44



##################################################################
####                        T-3
##################################################################

# Train data matrix
train_data_inputT3 <- matrix(,nrow = 0,ncol = 3)
colnames(train_data_inputT3) <- c("t1","t2","t3")
train_data_outputT3 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+3 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT3 <- rbind(train_data_inputT3,c(newInput)) 
  train_data_outputT3 <- append(train_data_outputT3,newOutput)
}
head(train_data_inputT3)

# create new dataframe with new inputs and new outputs
newDatasetT3 <- cbind(as.data.frame(train_data_inputT3),train_data_outputT3)
newDatasetT3

# training module
modelForLinearT3 <- neuralnet(train_data_outputT3 ~ t1 + t2 + t3, data=newDatasetT3,hidden = 5, linear.output = TRUE)

plot(modelForLinearT3)

# Test data matrix
test_data_inputT3 <- matrix(,nrow = 0,ncol = 3)
colnames(test_data_inputT3) <- c("t1","t2","t3")
test_data_outputT3 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+3 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT3 <- rbind(test_data_inputT3,c(newInput)) 
  test_data_outputT3 <- append(test_data_outputT3,newOutput)
}

# create test data frame
newDatasetForTestT3 <- as.data.frame(test_data_inputT3)

# Making Prediction
resultsT3 <- compute(modelForLinearT3,newDatasetForTestT3)

# result of neural network
resultT3 <- data.frame(actual = test_data_outputT3,prediction = resultsT3$net.result)

minOfDataSetT3 <- min(uow_load$`20`)
maxOfDatasetT3 <- max(uow_load$`20`)


# calculate accuracy
predictT3 = resultT3$prediction * abs(diff(range(test_data_outputT3))) + min(test_data_outputT3)
actualT3 = resultT3$actual * abs(diff(range(test_data_outputT3))) + min(test_data_outputT3)

comparisonT3 <- data.frame(predictT3,actualT3)
unnormalizeResultsT3 <- unnormalize(comparisonT3,minOfDataSetT3,maxOfDatasetT3)

deviationT3 <- (unnormalizeResultsT3$actualT3 - unnormalizeResultsT3$predictT3) / unnormalizeResultsT3$actualT3

comparisonT3 <- data.frame(unnormalizeResultsT3$predictT3,unnormalizeResultsT3$actualT3,deviationT3)
accuracyT3 = 1 - abs(mean(deviationT3))
accuracyT3

# get RMSE
RMSE_valueT3 <- rmse(unnormalizeResultsT3$actualT3,unnormalizeResultsT3$predictT3)
RMSE_valueT3

# get MAE 
MAE_valueT3 <- mae(unnormalizeResultsT3$actualT3,unnormalizeResultsT3$predictT3)
MAE_valueT3

# get MAPE
MAPE_valueT3 <- mape(unnormalizeResultsT3$actualT3, unnormalizeResultsT3$predictT3)
MAPE_valueT3

# get sMAPE
sMape_valueT3 <- smape(unnormalizeResultsT3$actualT3, unnormalizeResultsT3$predictT3)
sMape_valueT3

##################################################################
####                        T-2
##################################################################

# Train data matrix
train_data_inputT2 <- matrix(,nrow = 0,ncol = 2)
colnames(train_data_inputT2) <- c("t1","t2")
train_data_outputT2 <- c() # store output values


for(i in 1:length(trainData$X20)){
  day_counter <- i+2 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT2 <- rbind(train_data_inputT2,c(newInput)) 
  train_data_outputT2 <- append(train_data_outputT2,newOutput)
}

# create new dataframe with new inputs and new outputs
newDatasetT2 <- cbind(as.data.frame(train_data_inputT2),train_data_outputT2)

# training module
modelForLinear2 <- neuralnet(train_data_outputT2 ~ t1 + t2 , data=newDatasetT2,hidden = 5, linear.output = TRUE)

plot(modelForLinear2)

# Test data matrix
test_data_inputT2 <- matrix(,nrow = 0,ncol = 2)
colnames(test_data_inputT2) <- c("t1","t2")
test_data_outputT2 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+2 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT2 <- rbind(test_data_inputT2,c(newInput)) 
  test_data_outputT2 <- append(test_data_outputT2,newOutput)
}

# create test data frame
newDatasetForTestT2 <- as.data.frame(test_data_inputT2)

# Making Prediction
resultsT2 <- compute(modelForLinear2,newDatasetForTestT2)

# result of neural network
resultT2 <- data.frame(actual = test_data_outputT2,prediction = resultsT2$net.result)

minOfDataSet <- min(uow_load$`20`)
maxOfDataset <- max(uow_load$`20`)


# Denormalizing
unnormalize <- function(x,min,max){
  return((max-min)*x + min)
}

# calculate accuracy
predictT2 = resultT2$prediction * abs(diff(range(test_data_outputT2))) + min(test_data_outputT2)
actualT2 = resultT2$actual * abs(diff(range(test_data_outputT2))) + min(test_data_outputT2)

comparisonT2 <- data.frame(predictT2,actualT2)
unnormalizeResultsT2 <- unnormalize(comparisonT2,minOfDataSet,maxOfDataset)

deviationT2 <- (unnormalizeResultsT2$actualT2 - unnormalizeResultsT2$predictT2) / unnormalizeResultsT2$actualT2

comparisonT2 <- data.frame(unnormalizeResultsT2$predictT2,unnormalizeResultsT2$actualT2,deviationT2)
accuracyT2 = 1 - abs(mean(deviationT2))
accuracyT2

# get RMSE
RMSE_valueT2 <- rmse(unnormalizeResultsT2$actualT2,unnormalizeResultsT2$predictT2)
RMSE_valueT2

# get MAE 
MAE_valueT2 <- mae(unnormalizeResultsT2$actualT2,unnormalizeResultsT2$predictT2)
MAE_valueT2

# get MAPE
MAPE_valueT2 <- mape(unnormalizeResultsT2$actualT2, unnormalizeResultsT2$predictT2)
MAPE_valueT2

# get sMAPE
sMape_valueT2 <- smape(unnormalizeResultsT2$actualT2, unnormalizeResultsT2$predictT2)
sMape_valueT2


##################################################################
####                        T-1
##################################################################

# Train data matrix
train_data_inputT1 <- matrix(,nrow = 0,ncol = 1)
colnames(train_data_inputT1) <- c("t1")
train_data_outputT1 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+1 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT1 <- rbind(train_data_inputT1,c(newInput)) 
  train_data_outputT1 <- append(train_data_outputT1,newOutput)
}
head(train_data_inputT1)

# create new dataframe with new inputs and new outputs
newDatasetT1 <- cbind(as.data.frame(train_data_inputT1),train_data_outputT1)
newDatasetT1

# training module
modelForLinearT1 <- neuralnet(train_data_outputT1 ~ t1, data=newDatasetT1,hidden = 5, linear.output = TRUE)

plot(modelForLinearT1)

# Test data matrix
test_data_inputT1 <- matrix(,nrow = 0,ncol = 1)
colnames(test_data_inputT1) <- c("t1")
test_data_outputT1 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+1 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT1 <- rbind(test_data_inputT1,c(newInput)) 
  test_data_outputT1 <- append(test_data_outputT1,newOutput)
}

# create test data frame
newDatasetForTestT1 <- as.data.frame(test_data_inputT1)

# Making Prediction
resultsT1 <- compute(modelForLinearT1,newDatasetForTestT1)

# result of neural network
resultT1 <- data.frame(actual = test_data_outputT1,prediction = resultsT1$net.result)

minOfDataSetT1 <- min(uow_load$`20`)
maxOfDatasetT1 <- max(uow_load$`20`)


# calculate accuracy
predictT1 = resultT1$prediction * abs(diff(range(test_data_outputT1))) + min(test_data_outputT1)
actualT1 = resultT1$actual * abs(diff(range(test_data_outputT1))) + min(test_data_outputT1)

comparisonT1 <- data.frame(predictT1,actualT1)
unnormalizeResultsT1 <- unnormalize(comparisonT1,minOfDataSetT1,maxOfDatasetT1)

deviationT1 <- (unnormalizeResultsT1$actualT1 - unnormalizeResultsT1$predictT1) / unnormalizeResultsT1$actualT1

comparisonT1 <- data.frame(unnormalizeResultsT1$predictT1,unnormalizeResultsT1$actualT1,deviationT1)
accuracyT1 = 1 - abs(mean(deviationT1))
accuracyT1

# get RMSE
RMSE_valueT1 <- rmse(unnormalizeResultsT1$actualT1,unnormalizeResultsT1$predictT1)
RMSE_valueT1

# get MAE 
MAE_valueT1 <- mae(unnormalizeResultsT1$actualT1,unnormalizeResultsT1$predictT1)
MAE_valueT1

# get MAPE
MAPE_valueT1 <- mape(unnormalizeResultsT1$actualT1, unnormalizeResultsT1$predictT1)
MAPE_valueT1

# get sMAPE
sMape_valueT1 <- smape(unnormalizeResultsT1$actualT1, unnormalizeResultsT1$predictT1)
sMape_valueT1


##################################################################
####                    T-7 (5)
##################################################################

# Train data matrix
train_data_inputT7 <- matrix(,nrow = 0,ncol = 7)
colnames(train_data_inputT7) <- c("t1","t2","t3","t4","t5","t6","t7")
train_data_outputT7 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT7 <- rbind(train_data_inputT7,c(newInput)) 
  train_data_outputT7 <- append(train_data_outputT7,newOutput)
}
head(train_data_inputT7)

# create new dataframe with new inputs and new outputs
newDatasetT7 <- cbind(as.data.frame(train_data_inputT7),train_data_outputT7)
newDatasetT7

# training module
modelForLinearT7 <- neuralnet(train_data_outputT7 ~ t1 + t2 + t3 + t4 + t5 + t6 + t7, data=newDatasetT7,hidden = 5, linear.output = TRUE)

plot(modelForLinearT7)

# Test data matrix
test_data_inputT7 <- matrix(,nrow = 0,ncol = 7)
colnames(test_data_inputT7) <- c("t1","t2","t3","t4","t5","t6","t7")
test_data_outputT7 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT7 <- rbind(test_data_inputT7,c(newInput)) 
  test_data_outputT7 <- append(test_data_outputT7,newOutput)
}

# create test data frame
newDatasetForTestT7 <- as.data.frame(test_data_inputT7)

# Making Prediction
resultsT7 <- compute(modelForLinearT7,newDatasetForTestT7)

# result of neural network
resultT7 <- data.frame(actual = test_data_outputT7,prediction = resultsT7$net.result)

minOfDataSetT7 <- min(uow_load$`20`)
maxOfDatasetT7 <- max(uow_load$`20`)


# calculate accuracy
predictT7 = resultT7$prediction * abs(diff(range(test_data_outputT7))) + min(test_data_outputT7)
actualT7 = resultT7$actual * abs(diff(range(test_data_outputT7))) + min(test_data_outputT7)

comparisonT7 <- data.frame(predictT7,actualT7)
unnormalizeResultsT7 <- unnormalize(comparisonT7,minOfDataSetT7,maxOfDatasetT7)

deviationT7 <- (unnormalizeResultsT7$actualT7 - unnormalizeResultsT7$predictT7) / unnormalizeResultsT7$actualT7

comparisonT7 <- data.frame(unnormalizeResultsT7$predictT7,unnormalizeResultsT7$actualT7,deviationT7)
accuracyT7 = 1 - abs(mean(deviationT7))
accuracyT7

# get RMSE
RMSE_valueT7 <- rmse(unnormalizeResultsT7$actualT7,unnormalizeResultsT7$predictT7)
RMSE_valueT7

# get MAE 
MAE_valueT7 <- mae(unnormalizeResultsT7$actualT7,unnormalizeResultsT7$predictT7)
MAE_valueT7

# get MAPE
MAPE_valueT7 <- mape(unnormalizeResultsT7$actualT7, unnormalizeResultsT7$predictT7)
MAPE_valueT7

# get sMAPE
sMape_valueT7 <- smape(unnormalizeResultsT7$actualT7, unnormalizeResultsT7$predictT7)
sMape_valueT7

##################################################################
####                    T-7 (10)
##################################################################

# Train data matrix
train_data_inputT71 <- matrix(,nrow = 0,ncol = 7)
colnames(train_data_inputT71) <- c("t1","t2","t3","t4","t5","t6","t7")
train_data_outputT71 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT71 <- rbind(train_data_inputT71,c(newInput)) 
  train_data_outputT71 <- append(train_data_outputT71,newOutput)
}
head(train_data_inputT71)

# create new dataframe with new inputs and new outputs
newDatasetT71 <- cbind(as.data.frame(train_data_inputT71),train_data_outputT71)
newDatasetT71

# training module
modelForLinearT71 <- neuralnet(train_data_outputT7 ~ t1 + t2 + t3 + t4 + t5 + t6 + t7, data=newDatasetT7,hidden = 10, linear.output = TRUE)

plot(modelForLinearT71)

# Test data matrix
test_data_inputT71 <- matrix(,nrow = 0,ncol = 7)
colnames(test_data_inputT71) <- c("t1","t2","t3","t4","t5","t6","t7")
test_data_outputT71 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT71 <- rbind(test_data_inputT71,c(newInput)) 
  test_data_outputT71 <- append(test_data_outputT71,newOutput)
}

# create test data frame
newDatasetForTestT71 <- as.data.frame(test_data_inputT71)

# Making Prediction
resultsT71 <- compute(modelForLinearT71,newDatasetForTestT71)

# result of neural network
resultT71 <- data.frame(actual = test_data_outputT71,prediction = resultsT71$net.result)

minOfDataSetT71 <- min(uow_load$`20`)
maxOfDatasetT71 <- max(uow_load$`20`)


# calculate accuracy
predictT71 = resultT71$prediction * abs(diff(range(test_data_outputT71))) + min(test_data_outputT71)
actualT71 = resultT71$actual * abs(diff(range(test_data_outputT71))) + min(test_data_outputT71)

comparisonT71 <- data.frame(predictT71,actualT71)
unnormalizeResultsT71 <- unnormalize(comparisonT71,minOfDataSetT71,maxOfDatasetT71)

deviationT71 <- (unnormalizeResultsT71$actualT71 - unnormalizeResultsT71$predictT71) / unnormalizeResultsT71$actualT71

comparisonT71 <- data.frame(unnormalizeResultsT71$predictT71,unnormalizeResultsT71$actualT71,deviationT71)
accuracyT71 = 1 - abs(mean(deviationT71))
accuracyT71

# get RMSE
RMSE_valueT71 <- rmse(unnormalizeResultsT71$actualT71,unnormalizeResultsT71$predictT71)
RMSE_valueT71

# get MAE 
MAE_valueT71 <- mae(unnormalizeResultsT71$actualT71,unnormalizeResultsT71$predictT71)
MAE_valueT71

# get MAPE
MAPE_valueT71 <- mape(unnormalizeResultsT71$actualT71, unnormalizeResultsT71$predictT71)
MAPE_valueT71

# get sMAPE
sMape_valueT71 <- smape(unnormalizeResultsT71$actualT71, unnormalizeResultsT71$predictT71)
sMape_valueT71


##################################################################
####                    T-7 2 hidden (5,2)
##################################################################

# Train data matrix
train_data_inputT72 <- matrix(,nrow = 0,ncol = 7)
colnames(train_data_inputT72) <- c("t1","t2","t3","t4","t5","t6","t7")
train_data_outputT72 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT72 <- rbind(train_data_inputT72,c(newInput)) 
  train_data_outputT72 <- append(train_data_outputT72,newOutput)
}
head(train_data_inputT72)

# create new dataframe with new inputs and new outputs
newDatasetT72 <- cbind(as.data.frame(train_data_inputT72),train_data_outputT72)
newDatasetT72

# training module
modelForLinearT72 <- neuralnet(train_data_outputT72 ~ t1 + t2 + t3 + t4 + t5 + t6 + t7, data=newDatasetT72,hidden = c(5,2), linear.output = TRUE)

plot(modelForLinearT72)

# Test data matrix
test_data_inputT72 <- matrix(,nrow = 0,ncol = 7)
colnames(test_data_inputT72) <- c("t1","t2","t3","t4","t5","t6","t7")
test_data_outputT72 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT72 <- rbind(test_data_inputT72,c(newInput)) 
  test_data_outputT72 <- append(test_data_outputT72,newOutput)
}

# create test data frame
newDatasetForTestT72 <- as.data.frame(test_data_inputT72)

# Making Prediction
resultsT72 <- compute(modelForLinearT72,newDatasetForTestT72)

# result of neural network
resultT72 <- data.frame(actual = test_data_outputT72,prediction = resultsT72$net.result)

minOfDataSetT72 <- min(uow_load$`20`)
maxOfDatasetT72 <- max(uow_load$`20`)


# calculate accuracy
predictT72 = resultT72$prediction * abs(diff(range(test_data_outputT72))) + min(test_data_outputT72)
actualT72 = resultT72$actual * abs(diff(range(test_data_outputT72))) + min(test_data_outputT72)

comparisonT72 <- data.frame(predictT72,actualT72)
unnormalizeResultsT72 <- unnormalize(comparisonT72,minOfDataSetT72,maxOfDatasetT72)

deviationT72 <- (unnormalizeResultsT72$actualT72 - unnormalizeResultsT72$predictT72) / unnormalizeResultsT72$actualT72

comparisonT72 <- data.frame(unnormalizeResultsT72$predictT72,unnormalizeResultsT72$actualT72,deviationT72)
accuracyT72 = 1 - abs(mean(deviationT72))
accuracyT72

# get RMSE
RMSE_valueT72 <- rmse(unnormalizeResultsT72$actualT72,unnormalizeResultsT72$predictT72)
RMSE_valueT72

# get MAE 
MAE_valueT72 <- mae(unnormalizeResultsT72$actualT72,unnormalizeResultsT72$predictT72)
MAE_valueT72

# get MAPE
MAPE_valueT72 <- mape(unnormalizeResultsT72$actualT72, unnormalizeResultsT72$predictT72)
MAPE_valueT72

# get sMAPE
sMape_valueT72 <- smape(unnormalizeResultsT72$actualT72, unnormalizeResultsT72$predictT72)
sMape_valueT72

##################################################################
####                    T-7 2 hidden (10,7)
##################################################################

# Train data matrix
train_data_inputT73 <- matrix(,nrow = 0,ncol = 7)
colnames(train_data_inputT73) <- c("t1","t2","t3","t4","t5","t6","t7")
train_data_outputT73 <- c() # store output values

for(i in 1:length(trainData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(trainData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- trainData$X20[i:(day_counter-1)]
  newOutput <- trainData$X20[day_counter]
  
  train_data_inputT73 <- rbind(train_data_inputT73,c(newInput)) 
  train_data_outputT73 <- append(train_data_outputT73,newOutput)
}
head(train_data_inputT73)

# create new dataframe with new inputs and new outputs
newDatasetT73 <- cbind(as.data.frame(train_data_inputT73),train_data_outputT73)
newDatasetT73

# training module
modelForLinearT73 <- neuralnet(train_data_outputT73 ~ t1 + t2 + t3 + t4 + t5 + t6 + t7, data=newDatasetT73,hidden = c(10,7), linear.output = TRUE)

plot(modelForLinearT73)

# Test data matrix
test_data_inputT73 <- matrix(,nrow = 0,ncol = 7)
colnames(test_data_inputT73) <- c("t1","t2","t3","t4","t5","t6","t7")
test_data_outputT73 <- c() # store output values

for(i in 1:length(testData$X20)){
  day_counter <- i+7 # find the last record number
  
  # break loop if output value is out of bonds
  if(day_counter > length(testData$X20)){
    break
  }
  
  # assign new dataset 
  newInput <- testData$X20[i:(day_counter-1)]
  newOutput <- testData$X20[day_counter]
  
  test_data_inputT73 <- rbind(test_data_inputT73,c(newInput)) 
  test_data_outputT73 <- append(test_data_outputT73,newOutput)
}

# create test data frame
newDatasetForTestT73 <- as.data.frame(test_data_inputT73)

# Making Prediction
resultsT73 <- compute(modelForLinearT73,newDatasetForTestT73)

# result of neural network
resultT73 <- data.frame(actual = test_data_outputT73,prediction = resultsT73$net.result)

minOfDataSetT73 <- min(uow_load$`20`)
maxOfDatasetT73 <- max(uow_load$`20`)


# calculate accuracy
predictT73 = resultT73$prediction * abs(diff(range(test_data_outputT73))) + min(test_data_outputT73)
actualT73 = resultT73$actual * abs(diff(range(test_data_outputT73))) + min(test_data_outputT73)

comparisonT73 <- data.frame(predictT73,actualT73)
unnormalizeResultsT73 <- unnormalize(comparisonT73,minOfDataSetT73,maxOfDatasetT73)

deviationT73 <- (unnormalizeResultsT73$actualT73 - unnormalizeResultsT73$predictT73) / unnormalizeResultsT73$actualT73

comparisonT73 <- data.frame(unnormalizeResultsT73$predictT73,unnormalizeResultsT73$actualT73,deviationT73)
accuracyT73 = 1 - abs(mean(deviationT73))
accuracyT73

# get RMSE
RMSE_valueT73 <- rmse(unnormalizeResultsT73$actualT73,unnormalizeResultsT73$predictT73)
RMSE_valueT73

# get MAE 
MAE_valueT73 <- mae(unnormalizeResultsT73$actualT73,unnormalizeResultsT73$predictT73)
MAE_valueT73

# get MAPE
MAPE_valueT73 <- mape(unnormalizeResultsT73$actualT73, unnormalizeResultsT73$predictT73)
MAPE_valueT73

# get sMAPE
sMape_valueT73 <- smape(unnormalizeResultsT73$actualT73, unnormalizeResultsT73$predictT73)
sMape_valueT73







# Comparison Table
comparisonTable <- as.data.frame(matrix(nrow = 11, ncol = 5))
colnames(comparisonTable) <- c("Accuracy","RMSE","MAE","MAPE","sMAPE")
rownames(comparisonTable) <- c("t4(5)","t3(5)","t2(5)","t1(5)","t4(5,2)","t4(10)","t4(10,7)","t7(5)","t7(10)","t7(5,2)","t7(10,7)")

comparisonTable$Accuracy[1] <- accuracy
comparisonTable$RMSE[1] <- RMSE_value
comparisonTable$MAE[1] <- MAE_value
comparisonTable$MAPE[1] <- MAPE_value
comparisonTable$sMAPE[1] <- sMape_value

comparisonTable$Accuracy[2] <- accuracyT3
comparisonTable$RMSE[2] <- RMSE_valueT3
comparisonTable$MAE[2] <- MAE_valueT3
comparisonTable$MAPE[2] <- MAPE_valueT3
comparisonTable$sMAPE[2] <- sMape_valueT3

# T-2 (5)
comparisonTable$Accuracy[3] <- accuracyT2
comparisonTable$RMSE[3] <- RMSE_valueT2
comparisonTable$MAE[3] <- MAE_valueT2
comparisonTable$MAPE[3] <- MAPE_valueT2
comparisonTable$sMAPE[3] <- sMape_valueT2

# T-1 (5)
comparisonTable$Accuracy[4] <- accuracyT1
comparisonTable$RMSE[4] <- RMSE_valueT1
comparisonTable$MAE[4] <- MAE_valueT1
comparisonTable$MAPE[4] <- MAPE_valueT1
comparisonTable$sMAPE[4] <- sMape_valueT1

# T4 2 hiden layer (5,2)
comparisonTable$Accuracy[5] <- accuracyT42
comparisonTable$RMSE[5] <- RMSE_valueT42
comparisonTable$MAE[5] <- MAE_valueT42
comparisonTable$MAPE[5] <- MAPE_valueT42
comparisonTable$sMAPE[5] <- sMape_valueT42

# T4 hidden Layer(10)
comparisonTable$Accuracy[6] <- accuracyT43
comparisonTable$RMSE[6] <- RMSE_valueT43
comparisonTable$MAE[6] <- MAE_valueT43
comparisonTable$MAPE[6] <- MAPE_valueT43
comparisonTable$sMAPE[6] <- sMape_valueT43

# T4 2 hidden Layer(10,7)
comparisonTable$Accuracy[7] <- accuracyT44
comparisonTable$RMSE[7] <- RMSE_valueT44
comparisonTable$MAE[7] <- MAE_valueT44
comparisonTable$MAPE[7] <- MAPE_valueT44
comparisonTable$sMAPE[7] <- sMape_valueT44

# T7 hidden Layer 5
comparisonTable$Accuracy[8] <- accuracyT7
comparisonTable$RMSE[8] <- RMSE_valueT7
comparisonTable$MAE[8] <- MAE_valueT7
comparisonTable$MAPE[8] <- MAPE_valueT7
comparisonTable$sMAPE[8] <- sMape_valueT7

# T7 hidden Layer 10
comparisonTable$Accuracy[9] <- accuracyT71
comparisonTable$RMSE[9] <- RMSE_valueT71
comparisonTable$MAE[9] <- MAE_valueT71
comparisonTable$MAPE[9] <- MAPE_valueT71
comparisonTable$sMAPE[9] <- sMape_valueT71

# T7 hidden Layer (5,2)
comparisonTable$Accuracy[10] <- accuracyT72
comparisonTable$RMSE[10] <- RMSE_valueT72
comparisonTable$MAE[10] <- MAE_valueT72
comparisonTable$MAPE[10] <- MAPE_valueT72
comparisonTable$sMAPE[10] <- sMape_valueT72

# T7 hidden Layer (10,70)
comparisonTable$Accuracy[11] <- accuracyT73
comparisonTable$RMSE[11] <- RMSE_valueT73
comparisonTable$MAE[11] <- MAE_valueT73
comparisonTable$MAPE[11] <- MAPE_valueT73
comparisonTable$sMAPE[11] <- sMape_valueT73


comparisonTable
