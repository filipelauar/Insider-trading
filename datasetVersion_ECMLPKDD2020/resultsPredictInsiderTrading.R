library(data.table);
library(xgboost);


# Results for the best value of the weighed f1 recall
bestResult <- function(preds, dteste) 
{
  
  y_true <- getinfo(dteste, "label")
  
  DT <- data.table(y_true = y_true, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  numn <- length(y_true) - nump
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT <- DT[cleaner, ]
  DT[, recall := (tp_v / (tp_v + fp_v))]
  DT[, precision := (tp_v / (tp_v + fn_v))]
  DT[, f1sRecall := 2 * precision * recall^2 / (precision + recall)]
  
  best_row <- which.max(DT$f1sRecall)
  
  if (length(best_row) > 0) {
    return(list(metric = "f1sRecall", prob = DT$y_prob[best_row[1]], recall = DT$recall[best_row[1]], precision = DT$precision[best_row[1]], f1 = DT$f1s[best_row[1]], f1sPrecision = DT$f1sPrecision[best_row[1]], f1sRecall = DT$f1sRecall[best_row[1]]))
  } else {
    return(list(metric = "f1sRecall", value = -1))
  }
}

# Results by changing the threshold
allResults <- function(preds, dteste) 
{
  
  y_true <- getinfo(dteste, "label")
  
  DT <- data.table(y_true = y_true, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  numn <- length(y_true) - nump
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT <- DT[cleaner, ]
  DT[, recall := (tp_v / (tp_v + fp_v))]
  DT[, precision := (tp_v / (tp_v + fn_v))]
  DT[, f1s := 2 * precision * recall / (precision + recall)]
  DT[, f1sPrecision := 2 * precision * precision * recall / (precision + recall)]
  DT[, f1sRecall := 2 * precision * recall * recall / (precision + recall)]
  
  best_row <- which.max(DT$f1sRecall)
  
  if (length(best_row) > 0) {
    return(list(metric = "f1sRecall", prob = DT$y_prob, recall = DT$recall, precision = DT$precision, f1 = DT$f1s, positivePred = (DT$tp_v + DT$fp_v)))
  } else {
    return(list(metric = "f1sRecall", value = -1))
  }
}

# Evaluation metric for the algorithm optimization
evalF1Recall <- function(preds, dteste) 
{
  
  y_true <- getinfo(dteste, "label")
  
  DT <- data.table(y_true = y_true, y_prob = preds, key = "y_prob")
  cleaner <- !duplicated(DT[, "y_prob"], fromLast = TRUE)
  nump <- sum(y_true)
  numn <- length(y_true) - nump
  
  DT[, fp_v := cumsum(y_true == 1)]
  DT[, tp_v := nump - fp_v]
  DT[, fn_v := numn - as.numeric(cumsum(y_true == 0))]
  DT <- DT[cleaner, ]
  DT[, recall := (tp_v / (tp_v + fp_v))]
  DT[, precision := (tp_v / (tp_v + fn_v))]
  DT[, f1sRecall := 2 * precision * recall^2 / (precision + recall)]
  
  best_row <- which.max(DT$f1sRecall)
  
  if (length(best_row) > 0) {
    return(list(metric = "f1sRecall", value = DT$f1sRecall[best_row[1]]))
  } else {
    return(list(metric = "f1sRecall", value = -1))
  }
}

# The model
trainModel <- function(dtrain, dvalid)  
{
  p <- list(objective = "reg:logistic",
            booster = "gbtree",
            eval_metric = evalF1Recall,
            nthread = 7,
            eta = 0.0015,
            max_depth = 2,
            min_child_weight = 1,
            colsample_bytree = 0.80,
            colsample_bylevel = 0.50
  );
  
  watchlist <- list(train = dtrain, valid = dvalid);
  
  set.seed(1999);
  
  x <- xgb.train(params = p
                 , data = dtrain
                 , watchlist = watchlist
                 , nrounds = 4000
                 , print_every_n = 100
                 , early_stopping_rounds = 1000
                 , maximize = TRUE
                 , verbose = 1
  );
  
  return(x)
}

# Generate 100 random results and take precision and recall of the mean
generateDummyPredictions <- function(validLabels, testLabels) 
{
  #### Dummy classifier #####
  
  array_stratifiedPrecision = c()
  array_stratifiedRecall = c()
  array_uniformPrecision = c()
  array_uniformRecall = c()
  
  for(i in 1:100)
  {
    #stratified
    validClassDistribution = sum(validLabels)/length(validLabels)
    stratifiedRandomLabels = rep(0,length(testLabels))
    stratifiedRandomLabels[sample(length(stratifiedRandomLabels), validClassDistribution*length(stratifiedRandomLabels), replace = FALSE)] = 1
    
    stratifiedPrecision = sum(stratifiedRandomLabels[stratifiedRandomLabels == 1 & stratifiedRandomLabels == testLabels])/sum(stratifiedRandomLabels)
    stratifiedRecall = sum(stratifiedRandomLabels[stratifiedRandomLabels == 1 & stratifiedRandomLabels == testLabels])/sum(testLabels)
    
    array_stratifiedPrecision = c(array_stratifiedPrecision, stratifiedPrecision)
    array_stratifiedRecall = c(array_stratifiedRecall, stratifiedRecall)
    
    #uniform
    uniformRandomLabels = rep(0,length(testLabels))
    uniformRandomLabels[sample(length(uniformRandomLabels), 0.5*length(uniformRandomLabels), replace = FALSE)] = 1
    
    uniformPrecision = sum(uniformRandomLabels[uniformRandomLabels == 1 & uniformRandomLabels == testLabels])/sum(uniformRandomLabels)
    uniformRecall = sum(uniformRandomLabels[uniformRandomLabels == 1 & uniformRandomLabels == testLabels])/sum(testLabels)
    
    array_uniformPrecision = c(array_uniformPrecision, uniformPrecision)
    array_uniformRecall = c(array_uniformRecall, uniformRecall)
    
  }
  
  stratifiedPrecision = mean(array_stratifiedPrecision)
  stratifiedRecall = mean(array_stratifiedRecall)
  uniformPrecision = mean(array_uniformPrecision)
  uniformRecall = mean(array_uniformRecall)
  
  return(list(stratifiedPrecision, stratifiedRecall, uniformPrecision, uniformRecall))
}


#Include the path
#setwd('dataset_path')
data <- fread("dataset.csv")


#Split data respecting temporality
train = subset(data, data$date < '2017-07-11')
valid = subset(data, data$date > '2017-07-10' & data$date < '2017-12-29')
test = subset(data, data$date > '2017-12-29')

train.y <- train$insiderTraindgEvidence
valid.y <- valid$insiderTraindgEvidence
test.y <- test$insiderTraindgEvidence

#Exclude unused columns
train[,c(1,2,21:27)] = NULL
valid[,c(1,2,21:27)] = NULL
test[,c(1,2,21:27)] = NULL

dtrain  <- xgb.DMatrix(data = data.matrix(train), label = train.y);
dvalid  <- xgb.DMatrix(data = data.matrix(valid), label = valid.y);
dtest  <- xgb.DMatrix(data = data.matrix(test), label = test.y);


x <- trainModel(dtrain, dvalid)


predTestProbability <- predict(x, dtest);
predValidProbability <- predict(x, dvalid);


# Results for different threshold values
resultsTest <- allResults(predTestProbability, dtest)


probabilities = resultsTest$prob[resultsTest$recall > 0.05 & resultsTest$precision > .1]
trueRecall = resultsTest$recall[resultsTest$recall > 0.05 & resultsTest$precision > .1]
truePrecision = resultsTest$precision[resultsTest$recall > 0.05 & resultsTest$precision > .1]


# Metrics for the threshold that optimizes the weighed F1 Recall on the validation sample
resultValid <- bestResult(predValidProbability, dvalid)
predTestBinary <- ifelse(predTestProbability <= resultValid$prob, 0, 1);

print(paste0("Threshold: ", resultValid$prob))

#Metrics for the results
precision = MLmetrics::Precision(test.y, predTestBinary, positive = 1)
recall = MLmetrics::Recall(test.y, predTestBinary, positive = 1)
f1 = MLmetrics::F1_Score(test.y, predTestBinary, positive = 1)
f1Recall = f1 * recall


print(paste0('Precision: ',round(precision,digits = 3)))
print(paste0('Recall: ',round(recall,digits = 3)))
print(paste0('F1-score: ',round(f1,digits = 3)))
print(paste0('Weighed Recall F1-score: ',round(f1Recall,digits = 3)))

print("Best result for the test set: ")
print(bestResult(predTestProbability, dtest))


#Gerenating 100 random models and returning the mean of the precision and recall for each threshold
dummies <- generateDummyPredictions(valid.y, test.y)
meanStratifiedPrecision = dummies[[1]]
meanStratifiedRecall = dummies[[2]]
stratifiedF1 = 2 * meanStratifiedPrecision * meanStratifiedRecall / (meanStratifiedPrecision + meanStratifiedRecall)
stratifiedF1Recall = stratifiedF1 * meanStratifiedRecall


meanUniformPrecision = dummies[[3]]
meanUniformRecall = dummies[[4]]
uniformF1 = 2 * meanUniformPrecision * meanUniformRecall / (meanUniformPrecision + meanUniformRecall)
uniformF1Recall = uniformF1 * meanUniformRecall


print('Stratified')
print(paste0('Precision: ',round(meanStratifiedPrecision,digits = 3)))
print(paste0('Recall: ',round(meanStratifiedRecall,digits = 3)))
print(paste0('F1-score: ',round(stratifiedF1,digits = 3)))
print(paste0('Weighed Recall F1-score: ',round(stratifiedF1Recall,digits = 3)))


print('Uniform')
print(paste0('Precision: ',round(meanUniformPrecision,digits = 3)))
print(paste0('Recall: ',round(meanUniformRecall,digits = 3)))
print(paste0('F1-score: ',round(uniformF1,digits = 3)))
print(paste0('Weighed Recall F1-score: ',round(uniformF1Recall,digits = 3)))
