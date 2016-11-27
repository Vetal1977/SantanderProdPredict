source('data.sets.functions.R')

# read the train data from the file
train <- as.data.frame(
    fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)
)

# clean train data and get May 2015, June 2015, May 2016 sets separately
train.sets <- clean.and.split.train.df(train)

train.may.2015 <- train.sets[[1]]
train.june.2015 <- train.sets[[2]]
train.may.2016 <- train.sets[[3]]

# read and clean test data from the file
test <- as.data.frame(
    fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
              stringsAsFactors = FALSE)
)
test <- clean.test.df(test, train.may.2016)

# prepare train data for boost algorithm
train.june.2015 <- prepare.train.df.for.boost(train.may.2015, train.june.2015)

# create boost model
bst <- create.boost.model(train.june.2015)

# prediction
test <- make.prediction(test, bst, train.june.2015)

# get the results from the prediction
result <- get.result.df(test)

# prepare results to write
result_write <- prepare.result.to.write(result)

# save to csv
write.csv(result_write, 'result31.csv', quote = FALSE, row.names = FALSE)
