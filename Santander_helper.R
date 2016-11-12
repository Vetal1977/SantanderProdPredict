library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)

load.data <- function(filename) {
    data <- read.csv(filename, sep = ',', na.strings = 'NA', 
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
    data$cod_prov <- as.factor(data$cod_prov)
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
rm(train)
rm(train_last_month)
rm(products)
rm(products_ncodpers)
gc()

# load train data set with 'Maintained', 'Added', 'Dropped' status
train <- load.data('train_status_change.csv')

# filter 'Maintained' only
products <- grep("ind_+.*ult.*", names(train))
interesting <- rowSums(train[, products] != 'Maintained')
train <- train[interesting > 0,]

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

# convert product and status to factor
train$product <- as.factor(train$product)
added_product_count <- nrow(train[train$product_added == 1,])

# product popularity
train$product_popularity <- 0
product.popularity.df <- as.data.frame(
    train %>% 
    filter(product_added == 1) %>%
    group_by(product) %>%
        summarize(product_popularity = sum(product_added)/added_product_count))

# make age groups
train <- make.age.groups(train)

# make income groups
train <- make.income.groups(train)

# teach a model
model <- glm(product_added ~ pais_residencia + sexo + age_group +
                 ind_nuevo + segmento + ind_empleado +
                 ind_actividad_cliente + indresi + cod_prov +
                 income_group + product,
             family = binomial(link = 'logit'), data = train)
rm(train)
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
test[test$pais_residencia %in% c('AL', 'BA', 'BG', 'BZ', 'CD', 'CF', 'DJ', 'DZ', 
                                 'EC', 'EE', 'EG', 'GE', 'GH', 'GI', 'GM', 'GN', 
                                 'GT', 'GW', 'HR', 'HU', 'IS', 'JM', 'KH', 'KW', 
                                 'KZ', 'LB', 'LT', 'LV', 'LY', 'MD', 'MK', 'ML', 
                                 'MM', 'MR', 'MZ', 'NI', 'PH', 'PK', 'RS', 'SK', 
                                 'SL', 'TG', 'TH', 'TN', 'TW', 'UA', 'ZW'),]$pais_residencia <- 'ES'

# prediction
test_count <- nrow(test)
predicton_count <- as.integer(test_count / 1000000) + 1
test$product_added <- rep(0, nrow(test))
for (i in 1:predicton_count) {
    start_idx <- (i - 1)*1000000 + 1
    end_idx = min(c(i*1000000, test_count))
    print(c(start_idx, end_idx))
    to_predict <- test[start_idx : end_idx,]
    test[start_idx : end_idx,]$product_added <- predict(model, newdata = to_predict, type = 'response')
    gc()
}
rm(model)
rm(interesting)
rm(to_predict)
gc()

# order by code personal number and likelihood
test <- test[order(test$ncodpers, -test$product_added),]

# add product popularity
test <- merge(test, product.popularity.df, by.x = 'product', by.y = 'product')
test$combine_prediction <- 0.3 * test$product_added + 0.7 * test$product_popularity
test <- test[order(test$ncodpers, -test$combine_prediction),]

# select 5 most probable products for customers
test_dt <- data.table(test, key = c('ncodpers'))
result <- test_dt[, .SD[1:7], ncodpers]
rm(test)
gc()

# prepare results to write
result_write <- result %>% 
    group_by(ncodpers) %>% 
    summarise(added_products = paste(product, collapse = ' '))
result_write <- as.data.table(result_write)

# save to csv
write.csv(result_write, 'result6.csv', quote = FALSE, row.names = FALSE)
