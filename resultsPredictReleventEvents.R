library(data.table);
library(xgboost);

# Evaluation metric for the algorithm optimization
evalF1Precision <- function(preds, dteste) 
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
  DT[, f1sPrecision := 2 * precision^2 * recall / (precision + recall)]
  
  best_row <- which.max(DT$f1sPrecision)
  
  if (length(best_row) > 0) {
    return(list(metric = "f1sPrecision", value = DT$f1sPrecision[best_row[1]]))
  } else {
    return(list(metric = "f1sPrecision", value = -1))
  }
}

# The model
trainModel <- function(dtrain, dvalid)
{
  p <- list(objective = "reg:logistic", #amo.fairobj2, #"reg:logistic", #amo.fairobj2, #"reg:linear",
            booster = "gbtree", #"gblinear"
            eval_metric = evalF1Precision,
            nthread = 7,
            eta = 0.0015,
            max_depth = 2,
            min_child_weight = 2,
            colsample_bytree = 0.80,
            colsample_bylevel = 0.50
  );
  
  watchlist <- list(train = dtrain, valid = dvalid);
  
  set.seed(1999);
  
  x <- xgb.train(params = p
                 , data = dtrain
                 , watchlist = watchlist
                 , nrounds = 2000
                 , print_every_n = 100
                 , early_stopping_rounds = 1000
                 , maximize = TRUE
                 , verbose = 1
  );
  
  return(x)
}

# Results for the best value of the weighed f1 precision
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
  DT[, f1sPrecision := 2 * precision^2 * recall / (precision + recall)]
  
  DT <- DT[which(DT$recall >= 0.15)]
  best_row <- which.max(DT$f1sPrecision)
  
  if (length(best_row) > 0) {
    return(list(metric = "f1sPrecision", prob = DT$y_prob[best_row[1]], recall = DT$recall[best_row[1]], precision = DT$precision[best_row[1]], f1 = DT$f1s[best_row[1]], f1sPrecision = DT$f1sPrecision[best_row[1]]))
  } else {
    return(list(metric = "f1sPrecision", value = -1))
  }
}

# Label the event that has insider trading evidences related to it
includeNewsWithEvidence <- function(matriz)
{
  
  matriz$newsWithEvidence = 0
  i=0
  while(i < nrow(matriz))
  {
    i = i + 1
    
    if(matriz$insiderTraindgEvidence[i] == 1)
    {
      i = i + as.numeric(matriz$daysBeforeNewsEvent[i])
      matriz$newsWithEvidence[i] = 1
      
    }
  }
  
  return(matriz)
}

# Calculate the precision
calculatePrecision <- function(dataFrame, totalPredictions)
{
  
  #If the algorithm predicted a day in the time window, it predicted correct (precision)
  i=0
  outputWithEventNumber = 0
  while(i < nrow(dataFrame))
  {
    i=i+1
    if(dataFrame$newsWithEvidence[i] != 0)
    {
      for(j in time_window:1)
      {
        if(dataFrame[(i-j), 29] == 1)
        {
          outputWithEventNumber = outputWithEventNumber + 1
          dataFrame[i-j, 29] = 0
        }
      }
    }
  }
  
  precision = outputWithEventNumber / totalPredictions
  return(precision)
}

# Calculate the recall
calculateRecall <- function(dataFrame, eventsNumber)
{
  #If the algorithm predicted a day in the time window, it predicted the event (recall)
  i=0
  predictedEventsNumber = 0
  while(i < nrow(dataFrame))
  {
    i=i+1
    if(dataFrame$newsWithEvidence[i] != 0)
    {
      for(j in time_window:1)
      {
        if(dataFrame[i-j, 29] == 1)
        {
          predictedEventsNumber = predictedEventsNumber + 1
          break()
        }
      }
    }
  }
  
  recall = predictedEventsNumber / eventsNumber
  return(recall)
}

array_eventsNumber = c()
array_totalPredictions = c()
array_precision = c()
array_recall = c()


array_stratifiedPrecision = c()
array_stratifiedRecall = c()
array_uniformPrecision = c()
array_uniformRecall = c()

#Include the path
#setwd('dataset_path')
originalData = fread("dataset.csv")

for(time_window in c(20:1))
{
  data = originalData
  
  #Consider only evidences in the current time window
  data$insiderTraindgEvidence = ifelse(data$daysBeforeNewsEvent > time_window, 0, data$insiderTraindgEvidence)
  
  data = includeNewsWithEvidence(data)
  
  train = subset(data, data$date < '2017-07-11')
  valid = subset(data, data$date > '2017-07-10' & data$date < '2017-12-29')
  test = subset(data, data$date > '2017-12-29')
  
  train.y <- train$insiderTraindgEvidence
  valid.y <- valid$insiderTraindgEvidence
  test.y <- test$insiderTraindgEvidence
  
  #Exclude unused columns
  train[,c(1,2,21:28)] = NULL
  valid[,c(1,2,21:28)] = NULL
  test[,c(1,2,21:28)] = NULL
  
  
  dtrain  <- xgb.DMatrix(data = data.matrix(train), label = train.y);
  dvalid  <- xgb.DMatrix(data = data.matrix(valid), label = valid.y);
  dtest  <- xgb.DMatrix(data = data.matrix(test), label = test.y);
  
  
  
  x = trainModel(dtrain, dvalid)
  
  
  predTestProbability <- predict(x, dtest);
  predValidProbability <- predict(x, dvalid);
  
  resultValid = bestResult(predValidProbability, dvalid)
  
  predTestBinary <- ifelse(predTestProbability <= resultValid$prob, 0, 1);
  
  #Events with evidences on the test sample
  
  data2018 = subset(data, data$date > '2017-12-29')
  
  myData2018 = cbind.data.frame(data2018, predTestBinary)
  
  
  ##############  Calculating the precision and recall metrics for our algorithm ########
  
  eventsNumber = sum(data2018$newsWithEvidence == 1)
  totalPredictions = sum(predTestBinary)
  

  
  precision = calculatePrecision(myData2018, totalPredictions)
  recall = calculateRecall(myData2018, eventsNumber)
  
  
  array_eventsNumber = c(array_eventsNumber, eventsNumber)
  array_totalPredictions = c(array_totalPredictions, totalPredictions)
  array_precision = c(array_precision, precision)
  array_recall = c(array_recall, recall)
  
  
  
  ##############  Calculating the precision and recall metrics for a random model ########
  
  
  #Generate 100 random results

  array_stratifiedPrecisions = c()
  array_stratifiedRecalls = c()
  array_uniformPrecisions = c()
  array_uniformRecalls = c()

  for(i in 1:10)
  {

    #stratified
    validClassDistribution = sum(valid.y)/length(valid.y)
    stratifiedRandomLabels = rep(0,length(test.y))
    stratifiedRandomLabels[sample(length(stratifiedRandomLabels), validClassDistribution*length(stratifiedRandomLabels), replace = FALSE)] = 1

    randomData2018 = cbind.data.frame(data2018, stratifiedRandomLabels)

    randomPrecision = calculatePrecision(randomData2018, sum(stratifiedRandomLabels))
    randomRecall = calculateRecall(randomData2018, eventsNumber)

    array_stratifiedPrecisions = c(array_stratifiedPrecisions, randomPrecision)
    array_stratifiedRecalls = c(array_stratifiedRecalls, randomRecall)

    #uniform
    uniformRandomLabels = rep(0,length(test.y))
    uniformRandomLabels[sample(length(uniformRandomLabels), 0.5*length(uniformRandomLabels), replace = FALSE)] = 1

    randomData2018 = cbind.data.frame(data2018, uniformRandomLabels)

    randomPrecision = calculatePrecision(randomData2018, sum(uniformRandomLabels))
    randomRecall = calculateRecall(randomData2018, eventsNumber)

    array_uniformPrecisions = c(array_uniformPrecisions, randomPrecision)
    array_uniformRecalls = c(array_uniformRecalls, randomRecall)

  }
  #stratified
  array_stratifiedPrecision = c(array_stratifiedPrecision, mean(array_stratifiedPrecisions))
  array_stratifiedRecall = c(array_stratifiedRecall, mean(array_stratifiedRecalls))

  #uniform
  array_uniformPrecision = c(array_uniformPrecision, mean(array_uniformPrecisions))
  array_uniformRecall = c(array_uniformRecall, mean(array_uniformRecalls))

}


array_f1 = 2 * array_precision * array_recall / (array_precision + array_recall)
array_f1Precision = array_f1 * array_precision

array_stratifiedF1 = 2 * array_stratifiedPrecision * array_stratifiedRecall / (array_stratifiedPrecision + array_stratifiedRecall)
array_stratifiedF1Precision = array_stratifiedF1 * array_stratifiedPrecision

array_uniformF1 = 2 * array_uniformPrecision * array_uniformRecall / (array_uniformPrecision + array_uniformRecall)
array_uniformF1Precision = array_uniformF1 * array_uniformPrecision
