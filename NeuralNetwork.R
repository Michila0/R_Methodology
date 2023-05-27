library(neuralnet)               # to call the installed package
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)                   #create desired output

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)             #training dataset
colnames(trainingdata) <- c("Input","Output")
net.sqrt <- neuralnet(Output ~ Input, data = trainingdata, hidden = 6, act.fct = 'logistic', err.fct = 'sse', linear.output = T)
# look at the parameters inside the above code
net.sqrt
plot(net.sqrt)

testdata <- as.data.frame((1:10)^2) #Generate some squared numbers (i.e. testing dataset)
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#lets create the related desired output
testdata.result <- sqrt(testdata)
testdata.result
final_results <- cbind(testdata.result, net.results$net.result)
colnames(final_results) <- c("test", "NN_Result")
final_results

library(MLmetrics)
RMSE(final_results$test, final_results$NN_Result)

library(Metrics)
rmse(final_results$test, final_results$NN_Result)

cleanoutput <- cbind(testdata,sqrt(testdata), net.results$net.result)
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
cleanoutput


#NN Analysis
concrete <- read.csv("D:/IIT/Level5/Sem2/Machine Learning and Data Mining/week6/concrete.csv")  #use your own folder
str(concrete)
summary(concrete)      
# or you can check the summary of a specific variable, like the strength (which is our output variable). See below:
summary(concrete$strength)

library(ggplot2)
library(reshape2)
library(gridExtra)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
# later on we'll apply the reverse of normalization to the network output
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm)   # Check the summary for all normalized variables vs the previous summary

summary(concrete_norm$strength)   # summary of a specific normalized variable - strength
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

library(neuralnet)
library(grid)
library(MASS)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, hidden = 12, data = concrete_train, linear.output=TRUE)
plot(concrete_model)
