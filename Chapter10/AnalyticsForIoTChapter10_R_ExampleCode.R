###mice example code

#make sure all needed packages are installed
if(!require(mice)){
  install.packages("mice")
}
if(!require(VIM)){
  install.packages("VIM")
}
if(!require(lattice)){
  install.packages("lattice")
}

library(mice)
library(VIM)
library(lattice)

#load the airquality dataset (comes with R)
mice_example_data <- airquality 
#summarize original data.  Note NAs in Temp
summary(airquality)

#remove some data from the Temp field
mice_example_data[1:5,4] <- NA
#removed some data from the Wind field
mice_example_data[6:10,3] <-NA

#show summary, note the NA count for Temp
summary(mice_example_data)

#use mice to look at pattern of missing data. The first unnamed column shows the count of rows 
#with the complete or missing pattern as indicated by a 1 or 0 (missing) in the named columns
#The result shows 107 rows with complete data in all rows. 35 rows with only Ozone missing, 4 rows with only Solar.R missing, etc.
md.pattern(mice_example_data)

#Let's view it visually using the VIM package
aggr_plot <- aggr(mice_example_data, col=c('gray','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram - missing data","Pattern"))

#Remove the categorical variables for Month and Day before imputing.
#The mice() function is a Markov Chain Monte Carlo (MCMC) method that uses correlation of the data and 
#imputes missing values for each feature m times (default is 5) by using regression of incomplete variables
#on the other variables iteratively with the maximum iterations set by maxit.
imputed_example_data = mice(mice_example_data[-c(5,6)], m=5, printFlag=FALSE, maxit = 50, seed=250)

#view density plot of results.  Blue line is the observed actual data, the red lines are from the imputed data
densityplot(imputed_example_data)

#use the complete function to get the dataset with imputed values filled in
completed_example_data <- complete(imputed_example_data,1)
#now let's look at the summary
summary(completed_example_data)

#review imputation methods
methods(mice)

####for center and scaling example

###center and scale the completed example data
if(!require(caret)){
  install.packages("caret")
}
library(caret)
#create the object defining the pre-processing
cs_example_prepocessObj <- preProcess(completed_example_data, method = (c("center","scale"))) 
#apply the transformation
cs_example_data <- predict(cs_example_prepocessObj, completed_example_data)

#view summary of centered and scaled data
summary(cs_example_data)


###cross validation code example

#Create train control object that defines how training will be done.
#This example uses 10 fold cross validation which is repeated 5 time.
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                      repeats = 5)



###Random forest code example

#Add classes to the imputed data based on Temperature.
#This is what will be the target variable for the random forest model
cs_example_data$tempClass <- ifelse(cs_example_data$Temp > 0,"hot","cold")

#make sure needed packages are installed, then load them
if(!require(randomForest)){
  install.packages("randomForest")
}

library(randomForest)
library(caret)

#define how we are going to train the model
ctrlCV <- trainControl(method = "repeatedcv", number =10, repeats=5, returnResamp='none')

#define the target variable
target <- "tempClass"

#define the predictor features
predictors <- c("Ozone","Solar.R","Wind")

#split data into training and test
training <- createDataPartition(cs_example_data$tempClass, p=0.7, list=FALSE)
trainData <- cs_example_data[training,]
testData <- cs_example_data[-training,]

#train the random forest model and specify the number of trees.  Use caret to control cross-validation
rfModel <- train (trainData[,predictors],
                  trainData[,target],
                  method = "rf",
                  trControl = ctrlCV)

#run prediction on test data to get class probabilities
testPredRFProb <- predict(rfModel, testData, type = "prob")
#run prediction again to get predicted class
testData$RFclass <-  predict(rfModel, testData)

#grab the positive class probability (hot) and the predicted classes
testData$RFProb <- testPredRFProb[,"hot"]

#Show confusion matrix for results
confusionMatrix(data = testData$RFclass, reference = testData$tempClass, positive = "hot")

###Random forest code example end



###Gradient boosting machines code example

#make sure needed packages are installed, then load them
if(!require(gbm)){
  install.packages("gbm")
}

library(gbm)

#reuse train control and training data from random forest example

#train the random forest model and specify the number of trees.  Use caret to control cross-validation
gbmModel <- train (trainData[,predictors],
                  trainData[,target],
                  method = "gbm",
                  trControl = ctrlCV,
                  verbose = FALSE)

#run prediction on test data to get class probabilities
testPredGBMProb <- predict(gbmModel, testData, type = "prob")
#run prediction again to get predicted class
testData$GBMclass <-  predict(gbmModel, testData)

#grab the positive class probability (hot) and the predicted classes
testData$GBMProb <- testPredGBMProb[,"hot"]

#Show confusion matrix for results
confusionMatrix(data = testData$GBMclass, reference = testData$tempClass, positive = "hot")

###Gradient boosting machines code example end

###Anomaly detection example BEGIN

#load library
library(AnomalyDetection)

#This uses the vector method since no timestamp is present. If timestamp is present, use AnomalyDetectionTs
res = AnomalyDetectionVec(cs_example_data$Wind, max_anoms=0.05, period=60, direction='both', only_last=FALSE, plot=TRUE)
res$plot

###Anomaly detection example BEGIN


###Arima example code BEGIN

#load forecast library
library(forecast)

#create a time series on the Solar radiation data.  The air quality data was captured from May 1, 1973 to September 30, 1973
windTS <- ts(completed_example_data$Wind,start = c(1973,5,1), frequency = 365.25) 

#show values over time
plot.ts(windTS)

#fit an arima model
fit <- auto.arima(windTS)

#plot forecast for the next month
plot(forecast(fit, h=30))

###Arima sample code END
