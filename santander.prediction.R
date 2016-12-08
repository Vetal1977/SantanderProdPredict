source('data.sets.functions.R')

# read the train data from the file
train_orig <- as.data.frame(
    fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)
)

# clean train data and get May 2015, June 2015, May 2016 sets separately
train.sets <- clean.and.split.train.df(train_orig)
#rm(train_orig)
gc()

train.may.2015 <- train.sets[[1]]
train.june.2015 <- train.sets[[2]]
train.may.2016 <- train.sets[[3]]

# lagging
train.lagged <- train.sets[[4]]
train.lagged <- train.lagged[order(train.lagged$ncodpers, train.lagged$fecha_dato),]
train.lagged <- as.data.table(train.lagged)
products <- grep('ind_+.*ult1$', names(train.lagged), value = TRUE)
products.lag <- paste('lag', products, sep='.')
train.lagged[, paste(products.lag, 1:110, sep = '.') := shift(.SD), 
             by = ncodpers, 
             .SDcols = products]
train.june.2015.lagged <- as.data.frame(train.lagged[train.lagged$fecha_dato == '2015-06-28'])

# prepare train data for boost algorithm
train.june.2015 <- prepare.train.df.for.boost(train.may.2015, train.june.2015.lagged)
rm(train.june.2015.lagged)
gc()

train.june.2015[is.na(train.june.2015)] <- 0

# teach models

# convert outcome from factor to numeric matrix 
# xgboost takes multi-labels in [0, numOfClass)
num.class <- length(levels(train.june.2015$product))
product.lab <- as.matrix(as.integer(train.june.2015$product) - 1)

# prepare train matrix
train.june.2015.bst <- prepare.train.matrix(train.june.2015)

# set random seed, for reproducibility
set.seed(1234)

# train the model
param <- list('objective' = 'multi:softprob',    # multiclass classification 
              'num_class' = num.class,    # number of classes 
              'eval_metric' = 'mlogloss',    # evaluation metric 
              'nthread' = 8,   # number of threads to be used 
              'max_depth' = 8,    # maximum depth of tree 
              'eta' = 0.05,    # step size shrinkage 
              'gamma' = 0,    # minimum loss reduction 
              'subsample' = 0.7,    # part of data instances to grow tree 
              'colsample_bytree' = 0.7,  # subsample ratio of columns when constructing each tree 
              'min_child_weight' = 1  # minimum sum of instance weight needed in a child 
)
bst.cv <- xgb.cv(params = param, data = train.june.2015.bst, 
       label = product.lab, nrounds = 300, nfold = 4,
       prediction = TRUE, verbose = FALSE)
best.val <- min(bst.cv$dt$test.mlogloss.mean)
best.val.idx <- which.min(bst.cv$dt$test.mlogloss.mean)
gc()

bst <- xgboost(param = param, data = train.june.2015.bst, 
               label = product.lab, 
               nrounds = best.val.idx,
               verbose = FALSE)
gc()

#importance <- xgb.importance(train.june.2015.bst@Dimnames[[2]], model = bst)
#xgb.plot.importance(importance_matrix = head(importance, 20))

# read and clean test data from the file
test_orig <- as.data.frame(
    fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
          stringsAsFactors = FALSE)
)
test <- clean.test.df(test_orig, train.may.2016)
test.lagged <- train.sets[[5]]
test <- rbind(test.lagged, test)

test <- test[order(test$ncodpers, test$fecha_dato),]
test <- as.data.table(test)
gc()
products <- grep('ind_+.*ult1$', names(test), value = TRUE)
products.lag <- paste('lag', products, sep='.')
test[, paste(products.lag, 1:110, sep = '.') := shift(.SD), 
     by = ncodpers, 
     .SDcols = products]
test <- as.data.frame(test[test$fecha_dato == '2016-06-28'])

# prediction
test <- make.prediction(test, bst, train.june.2015)
products.lag <- grep('lag.ind_+.*ult.*', names(test))
test[, products.lag] <- NULL

# get the results from the prediction
result <- get.result.df(test)

# prepare results to write
result_write <- prepare.result.to.write(result)

# save to csv
write.csv(result_write, 'result48.csv', quote = FALSE, row.names = FALSE)
