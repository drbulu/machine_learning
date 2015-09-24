######## Analysis setup ###########################################################
set.seed(294)
require(caret)
source("./helper_functions.R")

######## Data Preparation #########################################################

#load data
trainData = read.csv("../data//pml-training.csv", stringsAsFactors=F)    
trainData$classe <- factor(trainData$classe)

## feature selection
rawSensorFeatures = grep("^accel|^gyros|^magn", names(trainData), value=T)
targetFeatures = rawSensorFeatures

# k-fold cross validation: randomly split trainData into 3 homogenous subsets
dataFoldSegments = createFolds(y = trainData$classe, k=3)

# subset data according to folds
targetFolds = list()
targetFolds$train = trainData[dataFoldSegments$Fold1,]
targetFolds$validation = trainData[dataFoldSegments$Fold2,]
targetFolds$test = trainData[dataFoldSegments$Fold3,]

# now to subset on features to remove unwanted features
targetFolds = subsetFeatures(folds = targetFolds, features = targetFeatures)

######## Training #################################################################

# Training the prediciton model. Random forest worked well in iniial testing
rfModel = train(classe ~ ., data=targetFolds$train, method="rf")
rfTest = testModel2(targetFolds, rfModel)

######## Testing ##################################################################

# load data
testData = read.csv("../data//pml-testing.csv", stringsAsFactors=F)

# filter the dataset to only the required features
featureColumns = grep( createGrepOrEntity(targetFeatures) , names(testData) ) 
holdoutTestData = testData[, featureColumns]

# make predictions on hold out test data
holdoutPredictions = predict(rfModel, holdoutTestData)

# export the predictions
answers = as.character(holdoutPredictions)
setwd("./results")

pml_write_files(answers)

