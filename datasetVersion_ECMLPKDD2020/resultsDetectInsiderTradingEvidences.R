library(data.table);
library(xgboost);


trainModel <- function(dtrain, dvalid)
{
  p <- list(objective = "reg:logistic",
            booster = "gbtree",
            eval_metric = 'auc',
            nthread = 7,
            eta = 0.0015,
            #scale_pos_weight = 20,
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
                 , nrounds = 20000
                 , print_every_n = 100
                 , early_stopping_rounds = 4000
                 , maximize = TRUE
                 , verbose = 1
  );
  
  return(x)
}

recall90 <- function(preds, dteste) 
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
  
  best_row <- rev(which(DT$recall >= 0.9))[1]
  
  if (length(best_row) > 0) {
    return(list(metric = "f1s", prob = DT$y_prob[best_row[1]], recall = DT$recall[best_row[1]], precision = DT$precision[best_row[1]], f1 = DT$f1s[best_row[1]], f1sPrecision = DT$f1sPrecision[best_row[1]], f1sRecall = DT$f1sRecall[best_row[1]]))
  } else {
    return(list(metric = "f1s", value = -1))
  }
}


#Include the path
#setwd('dataset_path')
data <- fread("dataset.csv")

#Subseting only day in the time window
data = subset(data ,data$daysBeforeNewsEvent != 0)

#Split data respecting temporality
train = subset(data, data$date < '2017-07-11')
valid = subset(data, data$date > '2017-07-10' & data$date < '2017-12-29')
test = subset(data, data$date > '2017-12-29')


train.y <- train$insiderTraindgEvidence
valid.y <- valid$insiderTraindgEvidence
test.y <- test$insiderTraindgEvidence

#Exclude unused columns
train[,c(1,2,24:27)] = NULL
valid[,c(1,2,24:27)] = NULL
test[,c(1,2,24:27)] = NULL


dtrain  <- xgb.DMatrix(data = data.matrix(train), label = train.y);
dvalid  <- xgb.DMatrix(data = data.matrix(valid), label = valid.y);
dtest  <- xgb.DMatrix(data = data.matrix(test), label = test.y);


x = trainModel(dtrain, dvalid)


predTestProbability <- predict(x, dtest);
predValidProbability <- predict(x, dvalid);

#Return the threshold with a Recall fo 90% on the valid sample
resultsValid = recall90(predValidProbability, dvalid)

#print the confusion matrix
predTestBinary <- ifelse(predTestProbability <= resultsValid$prob, 0, 1);
print(MLmetrics::ConfusionMatrix(predTestBinary, test.y))



