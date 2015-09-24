# function to cat the variables into a single REGEX 
createGrepOrEntity = function(x){
  output = x[1]
  if (length(x) > 1){ for(i in 2:length(x)){output = paste(output, x[i], sep="|")}}
  return (output)
}

### feature selection function
subsetFeatures = function(folds, features, outcome = "classe"){
  # create REGEX based on the variables (columns) to retain in data set
  varREGEX = createGrepOrEntity(c(outcome, features))
  # remove unwanted features from data folds (split data)
  folds$test <- folds$test[, grep( varREGEX, names(folds$test))]
  folds$train <- folds$train[, grep( varREGEX, names(folds$train))]
  # return processed data folds  
  return(folds)
}
# simple function to function to get the confusion matrix from the
# prediction made by a given model object on a particular data set
testModel = function(data, model){
  p = predict(model, data)
  c <- confusionMatrix(p1, data$classe)
  return (c)
}

testModel2 = function(folds, model){
  # self test on training fold
  p1 = predict(model, folds$train)
  c1 <- confusionMatrix(p1, folds$train$classe)
  # out of sample estimate on test fold
  p3 = predict(model, folds$test)
  c3 <- confusionMatrix(p3, folds$test$classe)
  # out of sample estimate on validation fold
  p2 = predict(model, folds$validation)
  c2 <- confusionMatrix(p2, folds$validation$classe)
  # creation of list to store output
  testData = list()
  testData$inSampleMat = c1
  testData$outSampleMat = c2
  testData$outSampleMat = c3
  testData$comparison = as.data.frame( rbind(inS = c1$overall,
                                             vS = c2$overall,
                                             outS = c3$overall
                                             # add the accuracy comparison table by class!
  ), stringsAsFactors=F)
  # return model testing info list
  return (testData)
}