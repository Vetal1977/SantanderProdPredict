library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(randomForest)

load.data <- function(filename) {
    data <- as.data.frame(
        fread(filename, sep = ';', na.strings = 'NA', 
                     stringsAsFactors = FALSE)
    )
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
    
    return(data)
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
    result[result$renta < 25000,]$income_group <- 1
    result[result$renta >= 25000 & result$renta < 45000,]$income_group <- 2
    result[result$renta >= 45000 & result$renta < 70000,]$income_group <- 3
    result[result$renta >= 70000 & result$renta < 90000,]$income_group <- 4
    result[result$renta >= 90000 & result$renta < 120000,]$income_group <- 5
    result[result$renta >= 120000 & result$renta < 160000,]$income_group <- 6
    result[result$renta >= 160000,]$income_group <- 7
    result$income_group <- as.factor(result$income_group)
    return(result)
}

# load train data from csv
train <- load.data('train_clean.csv')
train_last_month <- train[train$fecha_dato == '2016-05-28',]
products <- grep("ind_+.*ult.*", names(train))
products_ncodpers <- c(2, products)
train_last_month <- train_last_month[, products_ncodpers]

#load test data from csv
test <- load.data('test_clean.csv')
products <- grep("ind_+.*ult.*", names(test))
test <- test[, -products]
test <- test[order(test$ncodpers), ]

# merge product columns from train to test
test <- merge(test, train_last_month, by = c('ncodpers'))

# clean up
rm(train_last_month)
gc()

# calculate product popularity for individuals
product.popularity.individual <- as.data.table(train[, products_ncodpers])
product.popularity.individual <- product.popularity.individual[, lapply(.SD, sum), by = ncodpers]
cols <- colnames(product.popularity.individual)
rows_per_customer <- 17
product.popularity.individual[, 
                              cols[2:25] := lapply(.SD, function(x) x/rows_per_customer),
                              .SDcols = cols[2:25]]
gc()
product.popularity.individual <- as.data.frame(product.popularity.individual)
gc()

# calculate overall product popularity
product.popularity.overall <- as.data.table(product.popularity.individual[, cols[2:25]])
product.popularity.overall <- product.popularity.overall[, lapply(.SD, sum)]
cols <- colnames(product.popularity.overall)
customer_count <- nrow(product.popularity.individual)
product.popularity.overall[, 
                           cols[1:24] := lapply(.SD, function(x) x/customer_count),
                           .SDcols = cols[1:24]]

# rotate the product popularity 
product.popularity.individual <- product.popularity.individual %>% 
    melt(id = 1, measure = 2:25)
colnames(product.popularity.individual) <- c('ncodpers', 'product', 'popularity')
product.popularity.overall <- product.popularity.overall %>% 
    melt(measure = 1:24)
colnames(product.popularity.overall) <- c('product', 'popularity')
gc()

# load train data set with 'Maintained', 'Added', 'Dropped' status
train <- load.data('train_status_change.csv')

# filter 'Maintained' only
products <- grep("ind_+.*ult.*", names(train))
interesting <- rowSums(train[, products] != 'Maintained')
train <- train[interesting > 0,]
rm(interesting)
gc()

# 'rotate' the train data set where each row corresponds to the tripple customer ID - product - status
train <- train %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# remove unnecessary columns and all 'Maintained' products
train <- train[, !(names(train) %in% c('month_id', 'next_month_id', 'canal_entrada'))]
train <- filter(train, status != 'Maintained')

# create 'product added' column (1 if 'Added', 0 - otherwise)
train$product_added <- 0
train[train$status == 'Added',]$product_added <- 1
train <- train[, !(names(train) %in% c('status'))]

# merge with individual product popularity
product.popularity.individual$product <- as.character(product.popularity.individual$product)
train <- left_join(train, 
                   product.popularity.individual, 
                   by = c('ncodpers' = 'ncodpers', 'product' = 'product'))

# convert product and status to factor
train$product <- as.factor(train$product)

# make age groups
train <- make.age.groups(train)

# make income groups
train <- make.income.groups(train)

# cross validation
cross.validation.df <- train[train$fecha_dato == '2016-05-28',]
train.df <- train[train$fecha_dato != '2016-05-28',]

# teach models
model <- glm(product_added ~ age_group +
                 ind_nuevo + segmento + ind_empleado +
                 ind_actividad_cliente + nomprov +
                 income_group + product,
             family = binomial(link = 'logit'), 
             data = train.df)

model_prod_popularity <- lm(popularity ~ age_group +
                 ind_nuevo + segmento + #ind_empleado +
                 ind_actividad_cliente + nomprov +
                 renta + product, 
                 data = train.df)

#rm(train)
gc()

# 'rotate' test data
test <- test %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)
test <- test[test$status == 0,]

# make age groups
test <- make.age.groups(test)

# make income groups
test <- make.income.groups(test)

# convert product and status to factor
test$product <- as.factor(test$product)
test <- test[, !(names(test) %in% c('status', 'canal_entrada'))]

# unknown countries
#test[test$pais_residencia %in% c('AL', 'BA', 'BG', 'BZ', 'CD', 'CF', 'DJ', 'DZ', 
#                                 'EC', 'EE', 'EG', 'GE', 'GH', 'GI', 'GM', 'GN', 
#                                 'GT', 'GW', 'HR', 'HU', 'IS', 'JM', 'KH', 'KW', 
#                                 'KZ', 'LB', 'LT', 'LV', 'LY', 'MD', 'MK', 'ML', 
#                                 'MM', 'MR', 'MZ', 'NI', 'PH', 'PK', 'RS', 'SK', 
#                                 'SL', 'TG', 'TH', 'TN', 'TW', 'UA', 'ZW'),]$pais_residencia <- 'ES'
gc()

# prediction
test_count <- nrow(test)
predicton_count <- as.integer(test_count / 1000000) + 1
test$product_added <- 0
for (i in 1:predicton_count) {
    start_idx <- (i - 1)*1000000 + 1
    end_idx = min(c(i*1000000, test_count))
    print(c(start_idx, end_idx))
    to_predict <- test[start_idx : end_idx,]
    test[start_idx : end_idx,]$product_added <- predict(model, 
                                                        newdata = to_predict, 
                                                        type = 'response')
    rm(to_predict)
    gc()
}
#rm(model)
gc()

test$product_popularity_ind <- 0
for (i in 1:predicton_count) {
    start_idx <- (i - 1)*1000000 + 1
    end_idx = min(c(i*1000000, test_count))
    print(c(start_idx, end_idx))
    to_predict <- test[start_idx : end_idx,]
    tmp <- predict(model_prod_popularity, newdata = to_predict, interval = 'prediction')
    tmp <- as.data.frame(tmp)
    test[start_idx : end_idx,]$product_popularity_ind <- tmp$fit
    rm(to_predict)
    rm(tmp)
    gc()
}
#rm(model_prod_popularity)
gc()

# add product popularity (overall)
test <- merge(test, product.popularity.overall, by.x = 'product', by.y = 'product', all.x = TRUE)
gc()

# clean and scale personal product popularity
test[test$product_popularity_ind < 0,]$product_popularity_ind <- 0

# combine prediction
test$combine_prediction <- 0
test$combine_prediction <- 0.1 * test$product_added + 
    0.1 * test$product_popularity_ind
    0.8 * test$product_popularity
gc()
    
#test[is.na(test$product_popularity_ind_scaled),]$combine_prediction <- 
#    0.1 * test[is.na(test$product_popularity_ind_scaled),]$product_added + 
#    0.9 * test[is.na(test$product_popularity_ind_scaled),]$product_popularity

#test$combine_prediction <- 0.1 * test$product_added + 0.9 * test$product_popularity
#gc()

test <- test[order(test$ncodpers, -test$combine_prediction),]

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
write.csv(result_write, 'result16.csv', quote = FALSE, row.names = FALSE)
