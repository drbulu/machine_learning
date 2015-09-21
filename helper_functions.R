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
# function to simultaneously test all data subsets
testModel = function(folds, model){
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
  ), stringsAsFactors=F)
  # return model testing info list
  return (testData)
}

# Project submission helper from instruction page
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}