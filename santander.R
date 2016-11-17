library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)

product.status.change <- function(x) {
    if (length(x) == 1) {
        label = ifelse(x == 1, "Added", "Maintained")
    } 
    else {
        diffs <- diff(x) # difference month-by-month
        diffs <- c(0, diffs)
        label <- rep("Maintained", length(x))
        label <- ifelse(diffs == 1, 
                        "Added",
                        ifelse(diffs == -1, 
                               "Dropped", 
                               "Maintained"))
    }
    return(label)
}

# read the train data from the file
train <- fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)

# seems, we have 27734 useless rows with age, ind_nuevo, antiguedad and some other set to NA
# while other features set to ''. just remove them
train <- train[!is.na(train$age)]

# we have NA's for ind_nomina_ult1 & ind_nom_pens_ult1. set them to 0
train[is.na(train$ind_nomina_ult1)]$ind_nomina_ult1 <- 0
train[is.na(train$ind_nom_pens_ult1)]$ind_nom_pens_ult1 <- 0

# read test data from the file and combine them with train data for cleaning
test <- fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
              stringsAsFactors = FALSE)
combine <- rbind(train, test, fill = TRUE)
rm(train)
rm(test)
gc()

# limit ages
ageMedian <- median(combine[combine$age >= 18 & combine$age <= 90]$age)
combine[combine$age < 18 | combine$age > 90]$age <- ageMedian

# remove tipodom, conyuemp and
# remove ult_fec_cli_1t - last date as primary customer (if he isn't at the end of the month)
# we have code and name columns for province - remove duplicated information, province code
combine[, c('conyuemp', 'tipodom', 'ult_fec_cli_1t', 'cod_prov') := NULL]

# and replace empty province code with _U
combine$nomprov[combine$nomprov == ''] <- '_U'
gc()

# at the first step we calculate medians separately for Spain (separted for each province) and foreigners
medianIncomeForeign <- median(combine[!is.na(combine$renta) & combine$pais_residencia != 'ES']$renta)
combine[is.na(combine$renta) & combine$pais_residencia != 'ES']$renta <- medianIncomeForeign

medianSpain <- median(combine[!is.na(combine$renta) & combine$pais_residencia == 'ES']$renta)
incomesForSpain <- combine[!is.na(combine$renta) & combine$pais_residencia == 'ES', 
                         by = nomprov, 
                         lapply(.SD, median),
                         .SDcols = c('renta')]
tmp <- merge(combine[is.na(combine$renta) & combine$pais_residencia == 'ES'], 
             incomesForSpain, 
             all.x = TRUE,
             by = 'nomprov')
tmp$renta <- tmp$renta.y
tmp[, c('renta.x', 'renta.y') := NULL]
combine <- combine[!is.na(combine$renta)]
combine <- rbind(combine, tmp)
combine[is.na(combine$renta)]$renta <- medianSpain  # ES as country + 0 as province code
rm(tmp)
gc()

# replace empty strings
combine[combine$tiprel_1mes == '']$tiprel_1mes <- 'A'
combine[combine$indrel_1mes == '']$indrel_1mes <- '1'
combine[combine$indrel_1mes == 'P']$indrel_1mes <- '5'
combine$indrel_1mes <- as.integer(combine$indrel_1mes)
combine[combine$indrel_1mes > 5]$indrel_1mes <- 1
combine[is.na(combine$indrel_1mes)]$indrel_1mes <- 1
gc()

# replace empties
combine[combine$sexo == '']$sexo <- '_U'
combine[combine$canal_entrada == '']$canal_entrada <- '_U'
combine[combine$segmento == '']$segmento <- '_U'
gc()

# make dates and factors
combine$fecha_dato <- as.Date(combine$fecha_dato)
combine$fecha_alta <- as.Date(combine$fecha_alta)
combine$ind_empleado <- as.factor(combine$ind_empleado)
combine$pais_residencia <- as.factor(combine$pais_residencia)
combine$sexo <- as.factor(combine$sexo)
combine$ind_nuevo <- as.factor(combine$ind_nuevo)
combine$indrel <- as.factor(combine$indrel)
combine$indrel_1mes <- as.factor(combine$indrel_1mes)
combine$tiprel_1mes <- as.factor(combine$tiprel_1mes)
combine$indresi <- as.factor(combine$indresi)
combine$indext <- as.factor(combine$indext)
combine$canal_entrada <- as.factor(combine$canal_entrada)
combine$nomprov <- as.factor(combine$nomprov)
combine$ind_actividad_cliente <- as.factor(combine$ind_actividad_cliente)
combine$segmento <- as.factor(combine$segmento)
gc()

# split combine data set into train and test again
train <- combine[combine$fecha_dato != '2016-06-28']
gc()
test <- combine[combine$fecha_dato == '2016-06-28']
rm(combine)
gc()

write.table(train, 'train_clean.csv', quote = FALSE, row.names = FALSE, sep = ';')
write.table(test, 'test_clean.csv', quote = FALSE, row.names = FALSE, sep = ';')

#char.cols <- names(train)[sapply(train, is.character)]
#char.cols <- char.cols[!char.cols %in% c("fecha_alta")] #ignore dates for this purpose
#for (name in char.cols) {
#    print(sprintf("Unique values for %s:", name))
#    print(unique(train[[name]]))
#}

# remove all deceaseds and then the indfall column
#train <- train[train$indfall == 'N']
#train[, c('indfall') := NULL]

# introduce month IDs to apply the product status change from month to month
train <- train %>% arrange(fecha_dato)
train$month_id <- as.numeric(factor(train$fecha_dato))
train$next_month_id <- train$month_id + 1

# apply the product status change: each product column will contain 'Maintained', 'Added', 'Removed' values
products <- grep("ind_+.*ult.*", names(train))
train[, products] <- lapply(train[, products], 
                            function(x) {
                                    return(ave(x, train$ncodpers, FUN = product.status.change))
                                }
                            )
gc()

write.table(train, 'train_status_change.csv', quote = FALSE, row.names = FALSE, sep = ';')
gc()
train <- read.csv('train_status_change.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)
train$fecha_dato <- as.Date(train$fecha_dato)
train$fecha_alta <- as.Date(train$fecha_alta)
train$ind_empleado <- as.factor(train$ind_empleado)
train$pais_residencia <- as.factor(train$pais_residencia)
train$sexo <- as.factor(train$sexo)
train$ind_nuevo <- as.factor(train$ind_nuevo)
train$indrel <- as.factor(train$indrel)
train$indrel_1mes <- as.factor(train$indrel_1mes)
train$tiprel_1mes <- as.factor(train$tiprel_1mes)
train$indresi <- as.factor(train$indresi)
train$indext <- as.factor(train$indext)
train$canal_entrada <- as.factor(train$canal_entrada)
train$nomprov <- as.factor(train$nomprov)
train$ind_actividad_cliente <- as.factor(train$ind_actividad_cliente)
train$segmento <- as.factor(train$segmento)
train$indfall <- as.factor(train$indfall)

# convert data.frame to data.table
#train <- data.table(train)
#gc()

# divide train data set to train and cross validation
cross_valid <- train[train$fecha_dato == '2016-05-28',]
train <- train[train$fecha_dato != '2016-05-28',]
gc()

# remove rows with 'Maintained' products only since it is not interesting for learning
# actually we want to know when the status is changed to 'Added'

# remove row where _all_ products are only 'Maintained'
interesting <- rowSums(train[, products] != 'Maintained')
train <- train[interesting > 0,]

# 'rotate' the train data set where each row corresponds to the tripple customer ID - product - status
train <- train %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# 'rotate' the cross validation data set where each row corresponds to the tripple customer ID - product - status
cross_valid <- cross_valid %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# remove helper month_id and next_month_id
train <- train[, !(names(train) %in% c('month_id', 'next_month_id', 'canal_entrada'))]
cross_valid <- cross_valid[, !(names(cross_valid) %in% c('month_id', 'next_month_id'))]
gc()

# now we can remove all single products with status 'Maintained'
# !train here is a data.frame
train <- filter(train, status != "Maintained")

# convert data.frame to data.table
train <- data.table(train)
gc()

train$product <- as.factor(train$product)
train$status <- as.factor(train$status)
levels(train$status)[1] <- 1
levels(train$status)[2] <- 0
model <- glm(status ~ . - ncodpers, family = binomial(link = 'logit'), data = train)

cross_valid_status <- cross_valid$status

# MT (Malta) -> IT (Italy), SV (Salvador) -> MX (Mexiko) 
cross_valid[cross_valid$pais_residencia == 'MT']$pais_residencia <- 'IT'
cross_valid[cross_valid$pais_residencia == 'SV']$pais_residencia <- 'MX'

cross_valid[cross_valid$canal_entrada == 'KGN']$canal_entrada <- '_U'
cross_valid[cross_valid$canal_entrada == 'KHR']$canal_entrada <- '_U'

likelihood <- predict(model, newdata = train, type = 'response')
train$status_predict <- ifelse (likelihood > 0.2, '1', '2')
train_result <- nrow(train[train$status == train$status_predict,]) / nrow(train)

#cross_valid$status <- cross_valid_status
#cross_valid$status_predict <- ifelse (status_predict > 0.6, '2', '1')
#cross_valid_result <- nrow(cross_valid[cross_valid$status == cross_valid$status_predict]) / 
#    nrow(cross_valid)
#rm(cross_valid)

rm(train)
gc()

# prepare the test set similar to a train set
test <- read.csv('test_clean.csv', sep = ',', na.strings = 'NA', 
                  stringsAsFactors = FALSE)
test$fecha_dato <- as.Date(test$fecha_dato)
test$fecha_alta <- as.Date(test$fecha_alta)
test$ind_empleado <- as.factor(test$ind_empleado)
test$pais_residencia <- as.factor(test$pais_residencia)
test$sexo <- as.factor(test$sexo)
test$ind_nuevo <- as.factor(test$ind_nuevo)
test$indrel <- as.factor(test$indrel)
test$indrel_1mes <- as.factor(test$indrel_1mes)
test$tiprel_1mes <- as.factor(test$tiprel_1mes)
test$indresi <- as.factor(test$indresi)
test$indext <- as.factor(test$indext)
test$canal_entrada <- as.factor(test$canal_entrada)
test$nomprov <- as.factor(test$nomprov)
test$ind_actividad_cliente <- as.factor(test$ind_actividad_cliente)
test$segmento <- as.factor(test$segmento)
test$indfall <- as.factor(test$indfall)
test <- test %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)
gc()

test$product <- as.factor(test$product)
test[, c('status') := NULL]

# unknown countries
test[test$pais_residencia %in% c('AL', 'BA', 'BG', 'BZ', 'CD', 'CF', 'DJ', 'DZ', 
                                 'EC', 'EE', 'EG', 'GE', 'GH', 'GI', 'GM', 'GN', 
                                 'GT', 'GW', 'HR', 'HU', 'IS', 'JM', 'KH', 'KW', 
                                 'KZ', 'LB', 'LT', 'LV', 'LY', 'MD', 'MK', 'ML', 
                                 'MM', 'MR', 'MZ', 'NI', 'PH', 'PK', 'RS', 'SK', 
                                 'SL', 'TG', 'TH', 'TN', 'TW', 'UA', 'ZW')]$pais_residencia <- 'ES'

# unknow channels
test[test$canal_entrada %in% c('025', 'KBN', 'KBP', 'KCT', 'KCX', 'KDB', 
                               'KDI', 'KDL', 'KEQ', 'KEU', 'KFV', 'KGC', 
                               'KGU', 'KHS')]$canal_entrada <- '_U'
gc()

# prediction of more than 22 mio rows requires a waz too much memory, divide into chunks
#status_predict <- predict(model, newdata = test, type = 'response')

test_count <- nrow(test)
predicton_count <- as.integer(test_count / 1000000) + 1
test$status <- ''
gc()
for (i in 1:predicton_count) {
    start_idx <- (i - 1)*1000000 + 1
    end_idx = min(c(i*1000000, test_count))
    print(c(start_idx, end_idx))
    to_predict <- test[start_idx : end_idx]
    test[start_idx : end_idx]$likelihood <- predict(model, newdata = to_predict, type = 'response')
    gc()
}
gc()

# form the result
# we have many customers with predicted added products
result <- test %>% 
    filter(status == 'Added') %>% 
    group_by(ncodpers) %>% 
    summarise(added_products = paste(product, collapse = ' '))
result <- as.data.table(result)
gc()

# we have laos few customers with no added products - we need to bind them to the final set
result_dropped <- test %>% 
    filter(status == 'Dropped') %>% 
    group_by(ncodpers) %>% 
    summarise(dropped_products_count = n())
result_dropped <- as.data.table(result_dropped)
result_dropped <- result_dropped[result_dropped$dropped_products_count == 24]
result_dropped$added_products <- ''
result_dropped[, c('dropped_products_count') := NULL]
gc()

result <- rbind(result, result_dropped)
result <- result[order(ncodpers)]

# write results
write.csv(result, 'result2.csv', quote = FALSE, row.names = FALSE)
