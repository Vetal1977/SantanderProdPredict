library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(xgboost)

load.data <- function(filename) {
    data <- fread(filename, sep = ';', na.strings = 'NA', 
                     stringsAsFactors = FALSE)
    data$fecha_dato <- as.Date(data$fecha_dato)
    data$fecha_alta <- as.Date(data$fecha_alta)
    data$ind_empleado <- as.factor(data$ind_empleado)
    data$pais_residencia <- as.factor(data$pais_residencia)
    data$sexo <- as.factor(data$sexo)
    data$ind_nuevo <- as.factor(data$ind_nuevo)
    data$indrel <- as.factor(data$indrel)
    data$indrel_1mes <- as.factor(data$indrel_1mes)
    data$tiprel_1mes <- as.factor(data$tiprel_1mes)
    data$indresi <- as.factor(data$indresi)
    data$indext <- as.factor(data$indext)
    data$canal_entrada <- as.factor(data$canal_entrada)
    data$nomprov <- as.factor(data$nomprov)
    data$ind_actividad_cliente <- as.factor(data$ind_actividad_cliente)
    data$segmento <- as.factor(data$segmento)
    data$indfall <- as.factor(data$indfall)
    
    return(as.data.frame(data))
}

make.age.groups <- function(df) {
    result <- data.frame(df)
    
    result$age_group <- 0
    result[result$age < 30,]$age_group <- 1
    result[result$age >= 30 & result$age < 45,]$age_group <- 2
    result[result$age >= 45 & result$age < 60,]$age_group <- 3
    result[result$age >= 60 & result$age < 75,]$age_group <- 4
    result[result$age >= 75,]$age_group <- 5
    result$age_group <- as.factor(result$age_group)
    return(result)
}

make.income.groups <- function(df) {
    result <- data.frame(df)
    
    result$income_group <- 0
    result[result$renta < 15000,]$income_group <- 1
    result[result$renta >= 15000 & result$renta < 25000,]$income_group <- 2
    result[result$renta >= 25000 & result$renta < 40000,]$income_group <- 3
    result[result$renta >= 40000 & result$renta < 60000,]$income_group <- 4
    result[result$renta >= 60000 & result$renta < 80000,]$income_group <- 5
    result[result$renta >= 80000 & result$renta < 110000,]$income_group <- 6
    result[result$renta >= 110000 & result$renta < 130000,]$income_group <- 7
    result[result$renta >= 130000 & result$renta < 160000,]$income_group <- 8
    result[result$renta >= 160000 & result$renta < 200000,]$income_group <- 9
    result[result$renta >= 200000,]$income_group <- 10
    result$income_group <- as.factor(result$income_group)
    return(result)
}

prepare.predict.matrix <- function(df) {
    result <- df[, c('age', 'ind_nuevo', 'segmento',
                     'ind_empleado', 'ind_actividad_cliente',
                     'nomprov', 'renta')]
    result$segmento <- as.numeric(result$segmento)
    result$nomprov <- as.numeric(result$nomprov)
    result$ind_empleado <- as.numeric(result$ind_empleado)
    result <- as.matrix(result)
    mode(result) <- "numeric"
    return(result)
}

# load train data from csv
train_clean <- load.data('train_clean.csv')
train_clean_last_month <- train_clean[train_clean$fecha_dato == '2016-05-28',]
products <- grep("ind_+.*ult.*", names(train_clean))
products_ncodpers <- c(2, products)
train_clean_last_month <- train_clean_last_month[, products_ncodpers]

#load test data from csv
test <- load.data('test_clean.csv')
products <- grep("ind_+.*ult.*", names(test))
test <- test[, -products]
test <- test[order(test$ncodpers), ]

# merge product columns from train to test
test <- merge(test, train_clean_last_month, by = c('ncodpers'))

# clean up
rm(train_clean)
rm(train_clean_last_month)
rm(products)
rm(products_ncodpers)
gc()

# load train data set with 'Maintained', 'Added', 'Dropped' status
train <- load.data('train_status_change.csv')
train.june.2015 <- filter(train, fecha_dato == '2015-06-28')

# filter 'Maintained' only
products <- grep("ind_+.*ult.*", names(train.june.2015))
interesting <- rowSums(train.june.2015[, products] != 'Maintained')
train.june.2015 <- train.june.2015[interesting > 0,]
rm(interesting)

# 'rotate' the train data set where each row corresponds to the tripple customer ID - product - status
train.june.2015 <- train.june.2015 %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# remove unnecessary columns and all 'Maintained' products
train.june.2015 <- train.june.2015[, !(names(train.june.2015) %in% c('month_id', 'next_month_id'))]
train.june.2015 <- filter(train.june.2015, status == 'Added')

# convert product and status to factor
train.june.2015$product <- as.factor(train.june.2015$product)

# make age groups
train.june.2015 <- make.age.groups(train.june.2015)

# make income groups
train.june.2015 <- make.income.groups(train.june.2015)

# teach models

# convert outcome from factor to numeric matrix 
# xgboost takes multi-labels in [0, numOfClass)
num.class <- length(levels(train.june.2015$product))
product.lab <- as.matrix(as.integer(train.june.2015$product) - 1)

# xgboost parameters
param <- list("objective" = "multi:softprob",    # multiclass classification 
              "num_class" = num.class,    # number of classes 
              "eval_metric" = "mlogloss",    # evaluation metric 
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 8,    # maximum depth of tree 
              "eta" = 0.05,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 1,    # part of data instances to grow tree 
              "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 1  # minimum sum of instance weight needed in a child 
)

# prepare train matrix
train.june.2015.bst <- prepare.predict.matrix(train.june.2015)

# train the model
bst <- xgboost(param = param, data = train.june.2015.bst, 
               label = product.lab, nrounds = 50)
gc()

# make age groups
test <- make.age.groups(test)

# make income groups
test <- make.income.groups(test)
gc()

# unknown segment _U in test comparing to june 2015
test[test$segmento %in% c('_U'),]$segmento <- '02 - PARTICULARES'

# unknown employee index S in test comparing to june 2015
test[test$ind_empleado %in% c('S'),]$ind_empleado <- 'N'

# unknown products ind_ahor_fin_ult1, ind_aval_fin_ult1 in test comparing to june 2015
test <- test[, !(names(test) %in% c('ind_ahor_fin_ult1', 'ind_aval_fin_ult1'))]

# prediction

# preparation
to_predict <- prepare.predict.matrix(test)

# predic and interpret the results
pred <- predict(bst, newdata = to_predict)
pred <- matrix(pred, nrow=num.class, ncol=length(pred)/num.class)
pred <- t(pred)
colnames(pred) <- levels(train.june.2015$product)

# exclude preditions for already bought products
products <- grep("ind_+.*ult.*", names(test))
prod_status <- test[, products]
prod_status <- as.matrix(prod_status[, colnames(pred)])
prod_status <- (1 - prod_status)
pred <- prod_status * pred

# put predictions to test data.frame
test[, products] <- NULL
test <- cbind(test, pred)

# 'rotate' test data
test <- test %>%
    gather(key = product, value = prob, ind_cco_fin_ult1:ind_viv_fin_ult1)

# remove products with probability <= 0
test <- test[test$prob > 0,]

# sort by ncodpers and probability
test <- test[order(test$ncodpers, -test$prob),]

# select 7 most probable products for customers
test_dt <- data.table(test, key = c('ncodpers'))
result <- test_dt[, .SD[1:7], ncodpers]
gc()

# prepare results to write
result_write <- result %>% 
    group_by(ncodpers) %>% 
    summarise(added_products = paste(product, collapse = ' '))
result_write <- as.data.table(result_write)

# save to csv
write.csv(result_write, 'result22.csv', quote = FALSE, row.names = FALSE)
