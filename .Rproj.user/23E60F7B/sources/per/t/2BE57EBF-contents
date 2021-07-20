origData <- read.csv2('./Detailed_Statistics_Arrivals.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
head(origData)
nrow(origData)
dim(origData)
tail(origData)

# if you need to determine whether there is a correlation between fields in the data you can execute the 
# following function. In this example we wanna know whether column1 and column2 have the same value in all the
# data. If the returned value is 1 then both columns are perfectly correlated
#cor(origData[c('name-of-the-column1','name-of-column2')])

# find the number of mismatches in the data to determine if two columns have the same value
#mismatched <- origData[origData$Carrier.Code != origData$Tail.Number, ]
#nrow(mismatched)

# remove rows with NA not available or empty values
onTimeData <- origData[!is.na(origData$Arrival.Delay..Minutes.) & origData$Arrival.Delay..Minutes. != "", ]
nrow(onTimeData)

# changing data type in columns is easy
onTimeData$Scheduled.Elapsed.Time..Minutes. <- as.integer(onTimeData$Scheduled.Elapsed.Time..Minutes.)

# you could also use factors that are like enums to facilitate the work with your data
#onTimeData$Scheduled.Elapsed.Time..Minutes. <- as.factor(onTimeData$Scheduled.Elapsed.Time..Minutes.)

# to check the number of different values in your factor you can execute the following command
# tapply(onTimeData$Carrier.Code, onTimeData$Date..MM.DD.YYYY., length)
onTimeData$onTime <- ifelse(onTimeData$Arrival.Delay..Minutes. > 15, 0,1)
View(onTimeData)
#onTimeData[, c(onTimeData$Arrival.Delay..Minutes., onTimeData$onTime)]
#onTimeData[['Arrival.Delay..Minutes.','onTime']]
onTimeData[, c(10,18)]

# to solve the question we will be using the Logistic Regression in this case to determine with certain
# probability whether the flight will be delayed or not
# The decision factor to pick this algorithm was the following:
# Learning = Supervised
# Result = Binary classification
# Complexity = non ensemble algorithms (to avoid complexity)
# Basic

# we will use the caret package for classification and regression training
library(caret)

# setting the seed to ensure that we will always get the same training and testing data
set.seed(122515)

library(lubridate)
class(onTimeData$Date..MM.DD.YYYY.)
onTimeData$dayOfWeek <- wday(as.Date.character(onTimeData$Date..MM.DD.YYYY., '%m/%d/%y'), label = TRUE)
head(onTimeData)

# the list of colmns/features we will use to train our model. The columns we believe affect the flight delay
# get the date of the week
featureCols <- c("dayOfWeek","Origin.Airport","Wheels.on.Time","onTime")

# use a much simpler version of our data to train our model
onTimeDataFeatures <- onTimeData[, featureCols]
head(onTimeDataFeatures)

# we split our data in two groups and we will be using 70% of the data for training purposes
# sample data for training purposes
inTrainRows <- createDataPartition(onTimeDataFeatures$onTime, p=0.70, list = FALSE)
View(inTrainRows)

library(stringr)
library(chron)

#block_check <- function(range_start, range_end, block_start) {
#  start_check <- (range_start < block_start + times("01:00:00"))
#  end_check <- (range_end > block_start)
#  if (start_check & end_check) {
#    o_start <- max(block_start, range_start)
#    o_end <- min(block_start + times("01:00:00"), range_end)
#    return(o_end - o_start)
#  } else {
#    return(times("00:00:00"))
#  }
#}

getTimeBlock <- function(time) {
  array <- str_split(time, ":")[[1]]
  
  if (as.integer(array[[2]]) >= 30 ) {
    return (paste(array[[1]],":30", sep = ""))
  } else {
    return (paste(array[[1]],":00", sep = ""))
  }
}

# fixing a problem with the data. There is no 24:00 time
#onTimeDataFeatures[828, ]$Wheels.on.Time <- "23:59:00"
#onTimeDataFeatures[828,]$Wheels.on.Time.with.seconds <- "23:59:00"

#onTimeDataFeatures$Wheels.on.Time.with.seconds <- str_c(onTimeDataFeatures$Wheels.on.Time, ":00")
#onTimeDataFeatures$Wheels.on.Time <- ifelse(onTimeDataFeatures$Wheels.on.Time == "24:00", "23:59", 
#                                      onTimeDataFeatures$Wheels.on.Time)
#onTimeDataFeatures$Wheels.on.Time.with.seconds <- ifelse(onTimeDataFeatures$Wheels.on.Time.with.seconds == "24:00:00", 
#                                      "23:59:00", onTimeDataFeatures$Wheels.on.Time.with.seconds)



#blocks <- str_c(as.character(0:23), ":00:00")
#f <- function(block) { 
#  z <- mapply(FUN = block_check, times(onTimeDataFeatures$Wheels.on.Time.with.seconds), times("23:59:00"), 
#              MoreArgs = list(block_start = block)) 


#  times(z)
#}

blocks <- onTimeDataFeatures$Wheels.on.Time
blockTimes <- c()
for (block in blocks) {
  blockTimes <- c(blockTimes, getTimeBlock(block) ) 
}

onTimeDataFeatures$Wheels.on.Time.Block <- blockTimes
onTimeDataFeatures$Wheels.on.Time.with.seconds <- NULL
onTimeDataFeatures$Wheels.on.Time.Block <- as.factor(onTimeDataFeatures$Wheels.on.Time.Block)

featureCols <- c("dayOfWeek","Origin.Airport","Wheels.on.Time.Block","onTime")

# use a much simpler version of our data to train our model
onTimeDataFeaturesWithTimeBlocks <- onTimeDataFeatures[, featureCols]

onTimeDataFeaturesWithTimeBlocks$onTime <- as.factor(onTimeDataFeaturesWithTimeBlocks$onTime)

#onTimeDataFeatures$Wheels.on.Time.Block <- getTimeBlock(onTimeDataFeatures$Wheels.on.Time)

onTimeDataTraining <- onTimeDataFeaturesWithTimeBlocks[inTrainRows,]
onTimeDataTraining

# get the data to test the model. Sample data for testing
onTimeDataTesting <- onTimeDataFeaturesWithTimeBlocks[-inTrainRows,]
onTimeDataTesting

# I'm removing one row from the data frame that is not present at the training data set because the
# model is complaining during the prediction because of the new value
onTimeDataTestingTrimmed <- onTimeDataTesting[-c(30),]

nrow(onTimeDataFeaturesWithTimeBlocks)
nrow(onTimeDataTraining)
nrow(onTimeDataTestingTrimmed)

# the percentage of data for training purposes
nrow(onTimeDataTraining)/nrow(onTimeDataFeaturesWithTimeBlocks)
# the percentage of data for testing purposes
nrow(onTimeDataTestingTrimmed)/nrow(onTimeDataFeaturesWithTimeBlocks)

# creating the training model. Notice the first parameter of the function is the value that we want to 
# predict (onTime) and after the ~ character we put all the columns/features we want to use to predict
# the value. Then we specify the training data, the algorithm (in this case logistic regression), so R
# provides us the algorithm referencing it with the glm string and the family parameter binomial
logisticRegressionModel <- train(onTime ~ ., data = onTimeDataTraining, method="glm", family="binomial")
logisticRegressionModel

# now test our model to see how accurate it is
logisticRegressionPrediction <- predict(logisticRegressionModel, onTimeDataTestingTrimmed)
logisticRegressionPrediction

logRegressionConfussionMatrix <- confusionMatrix(logisticRegressionPrediction, 
                                                 onTimeDataTestingTrimmed[,"onTime"])

# the output of the model is read it in the following way
#          Reference
#Prediction   0   1
#          0  16   6
#          1  37 198
#          Reference
#Prediction   0   1
#          0  A   B
#          1  C   D

# Notice that the predictions are the rows while the reference (the test data are the columns), so
# The number of flights in the test data that the model accurately predicted as not on time was 16 while
# the number of flights the model predicted as not on time that actually were on time in the testing data
# was 6.
# The number of flights in the test data that the model wrongly predicted as on time was 37 while the number
# of flights the model accurately predicted as on time in the test data was 198
# So that's why the accuracy of the model is 83.27% The formula is Accuracy = A+D / # of test rows
# = (16+198)/16+6+37+198 = 0.8327
# Sensitivity = A/(A+C) = 16/(16+37) = 0.30189 Sensitivity indicates how the model predict delay when
# there is delay
# specificity is the ability of the model to predict no delay (on time) when there is no delay (on time)
# specificity = D/(B+D) = 198/(6+198)
# the positive predictive value predicts when there will be delay is 0.72
# the formula is PPV = (Sensitivity * Prevalence) / ((Sensitivity * Prevalence) + ((1-Specificity) * (1-Prevalence)))
# where Prevalence = (A+C)/(A+B+C+D)
# the negative predictive value predicts when there will be no delay is 0.84
# the formula is NPV = (Specificity * (1-Prevalence)) / (((1-Sensitivity) * Prevalence) + ((Specificity) * (1-Prevalence)))

# Lets try another algorithm... the random forest one of the more complex ensemble algorithms
library(randomForest)

rfModel <- randomForest(onTime ~ ., onTimeDataTraining, proximity = TRUE, importance = TRUE)
rfModel

rfValidation <- predict(rfModel, onTimeDataTestingTrimmed)

# evaluate the performance of the model getting the confusion matrix
rfConfMatrix <- confusionMatrix(rfValidation, onTimeDataTestingTrimmed[,"onTime"])
# wow the accuracy of the model using the random forest algorithm went is almost the same one 83.66%
rfConfMatrix

###########################
featureCols <- c("Origin.Airport","onTime")
onTimeDataTraining2 <- onTimeDataTraining[,featureCols]
onTimeDataTraining2$dayOfWeek <- as.character(onTimeDataTraining$dayOfWeek)
onTimeDataTraining2$Wheels.on.Time.Block <- as.character(onTimeDataTraining$Wheels.on.Time.Block)

onTimeDataTestingTrimmed2 <- onTimeDataTestingTrimmed[, featureCols]
onTimeDataTestingTrimmed2$dayOfWeek <- as.character(onTimeDataTestingTrimmed$dayOfWeek)
onTimeDataTestingTrimmed2$Wheels.on.Time.Block <- as.character(onTimeDataTestingTrimmed$Wheels.on.Time.Block)

rfModel2 <- randomForest(onTime ~ ., onTimeDataTraining2, proximity = TRUE, importance = TRUE)
rfModel2

rfValidation2 <- predict(rfModel2, onTimeDataTestingTrimmed2)

# evaluate the performance of the model getting the confusion matrix
rfConfMatrix2 <- confusionMatrix(rfValidation2, onTimeDataTestingTrimmed2[,"onTime"])
# wow the accuracy of the model using the random forest algorithm went is almost the same one 83.66%
rfConfMatrix2

# try to predict a value for a given data (use your model)
predictors <- data.frame(
  dayOfWeek = "Sat",
  Origin.Airport="DFW",
  Wheels.on.Time.Block="00:00"
)

predict(
  object = rfModel2,
  newdata = predictors,
  #proximity = TRUE,
  #importance=TRUE,
  type = "prob"
)
