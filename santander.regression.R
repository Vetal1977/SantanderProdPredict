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

prepare.predict.matrix <- function(df) {
    result <- df[, c('age', 'ind_nuevo', 'segmento',
                     'ind_empleado', 'ind_actividad_cliente',
                     'nomprov', 'renta',
                     'antiguedad', 'indrel',
                     'tiprel_1mes', 'sexo',
                     'indfall', 'canal_entrada',
                     'indext', 'indrel_1mes',
                     'indresi', 'pais_residencia')]
    result$segmento <- as.numeric(result$segmento)
    result$nomprov <- as.numeric(result$nomprov)
    result$ind_empleado <- as.numeric(result$ind_empleado)
    result$tiprel_1mes <- as.numeric(result$tiprel_1mes)
    result$sexo <- as.numeric(result$sexo)
    result$indfall <- as.numeric(result$indfall)
    result$canal_entrada <- as.numeric(result$canal_entrada)
    result$indext <- as.numeric(result$indext)
    result$indrel_1mes <- as.numeric(result$indrel_1mes)
    result$indresi <- as.numeric(result$indresi)
    result$pais_residencia <- as.numeric(result$pais_residencia)
    #result$fecha_alta <- as.numeric(factor(result$fecha_alta))
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
rm(train_clean_last_month)
rm(products)
rm(products_ncodpers)
gc()

# load train data set with 'Maintained', 'Added', 'Dropped' status
#train <- load.data('train_status_change.csv')
train.may.2015 <- filter(train_clean, fecha_dato == '2015-05-28')
train.june.2015 <- filter(train_clean, fecha_dato == '2015-06-28')

# filter 'Maintained' only
products <- grep("ind_+.*ult.*", names(train.june.2015))

products_ncodpers <- c(2, products)
train.may.2015 <- train.may.2015[, products_ncodpers]

interesting <- rowSums(train.june.2015[, products])
train.june.2015 <- train.june.2015[interesting > 0,]

# 'rotate' the train data set where each row corresponds to the tripple customer ID - product - status
train.may.2015 <- train.may.2015 %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)
train.june.2015 <- train.june.2015 %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# remove unnecessary columns and all 'Maintained' products
#train.june.2015 <- train.june.2015[, !(names(train.june.2015) %in% c('month_id', 'next_month_id'))]
train.june.2015 <- filter(train.june.2015, status > 0)
train.june.2015 <- merge(
    train.june.2015, 
    train.may.2015, 
    by = c('ncodpers', 'product'),
    all.x = TRUE)
#train.june.2015[is.na(train.june.2015$status.y),]$status.y <- 0
train.june.2015 <- train.june.2015[!is.na(train.june.2015$status.y),]
train.june.2015$added <- train.june.2015$status.x - train.june.2015$status.y
train.june.2015 <- filter(train.june.2015, added > 0)
train.june.2015$status.x <- NULL
train.june.2015$status.y <- NULL

# convert product and status to factor
train.june.2015$product <- as.factor(train.june.2015$product)

# teach models

# convert outcome from factor to numeric matrix 
# xgboost takes multi-labels in [0, numOfClass)
num.class <- length(levels(train.june.2015$product))
product.lab <- as.matrix(as.integer(train.june.2015$product) - 1)

# prepare train matrix
train.june.2015.bst <- prepare.predict.matrix(train.june.2015)

# set random seed, for reproducibility
set.seed(1234)

# train the model
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

bst <- xgboost(param = param, data = train.june.2015.bst, 
               label = product.lab, nrounds = 50)
gc()

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
test.rotated <- test %>%
    gather(key = product, value = prob, ind_cco_fin_ult1:ind_viv_fin_ult1)

# remove products with probability <= 0
test.rotated <- test.rotated[test.rotated$prob > 0,]

# sort by ncodpers and probability
test.rotated <- test.rotated[order(test.rotated$ncodpers, -test.rotated$prob),]

# select 7 most probable products for customers
test_dt <- data.table(test.rotated, key = c('ncodpers'))
result <- test_dt[, .SD[1:7], ncodpers]
gc()

# prepare results to write
result_write <- result %>% 
    group_by(ncodpers) %>% 
    summarise(added_products = paste(product, collapse = ' '))
result_write <- as.data.table(result_write)

# save to csv
write.csv(result_write, 'result28.csv', quote = FALSE, row.names = FALSE)
