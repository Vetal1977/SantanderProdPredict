library(data.table)
library(dplyr)
library(tidyr)

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
combine[combine$age < 18 & combine$age > 90]$age <- ageMedian

# remove tipodom, conyuemp and
# remove ult_fec_cli_1t - last date as primary customer (if he isn't at the end of the month)
combine[, c('conyuemp', 'tipodom', 'ult_fec_cli_1t') := NULL]

# we have code and name columns for province - remove duplicated information, province name
# and replace NA province code with 0
combine[, c('nomprov') := NULL]
combine[is.na(combine$cod_prov)]$cod_prov <- 0
gc()

# at the first step we calculate medians separately for Spain (separted for each province) and foreigners
medianIncomeForeign <- median(combine[!is.na(combine$renta) & combine$pais_residencia != 'ES']$renta)
combine[is.na(combine$renta) & combine$pais_residencia != 'ES']$renta <- medianIncomeForeign

medianSpain <- median(combine[!is.na(combine$renta) & combine$pais_residencia == 'ES']$renta)
incomesForSpain <- combine[!is.na(combine$renta) & combine$pais_residencia == 'ES', 
                         by = cod_prov, 
                         lapply(.SD, median),
                         .SDcols = c('renta')]
tmp <- merge(combine[is.na(combine$renta) & combine$pais_residencia == 'ES'], 
             incomesForSpain, 
             all.x = TRUE,
             by = 'cod_prov')
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
gc()

# remove all deceaseds and then the indfall column
combine <- combine[combine$indfall == 'N']
combine[, c('indfall') := NULL]

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
combine$cod_prov <- as.factor(combine$cod_prov)
combine$ind_actividad_cliente <- as.factor(combine$ind_actividad_cliente)
combine$segmento <- as.factor(combine$segmento)
gc()

# split combine data set into train and test again
train <- combine[combine$fecha_dato != '2016-06-28']
gc()
test <- combine[combine$fecha_dato == '2016-06-28']
rm(combine)
gc()

#char.cols <- names(train)[sapply(train, is.character)]
#char.cols <- char.cols[!char.cols %in% c("fecha_alta")] #ignore dates for this purpose
#for (name in char.cols) {
#    print(sprintf("Unique values for %s:", name))
#    print(unique(train[[name]]))
#}

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

# remove rows with 'Maintained' products only since it is not interesting for learning
# actually we want to know when the status is changed to 'Added'

# remove row where _all_ products are only 'Maintained'
interesting <- rowSums(train[, products] != 'Maintained')
train <- train[interesting > 0,]

# 'rotate' the data set where each row corresponds to the tripple customer ID - product - status
train <- train %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)

# now we can remove all single products with status 'Maintained'
# !train here is a data.frame
train <- filter(train, status != "Maintained")

# convert data.frame to data.table
train <- data.table(train)

# remove helper month_id and next_month_id
train[, c('month_id', 'next_month_id', 'ncodpers') := NULL]
gc()

# divide train data set to train and cross validation
#cross_valid <- train[train$fecha_dato == '2016-05-28']
#train <- train[train$fecha_dato != '2016-05-28']
gc()

train$product <- as.factor(train$product)
train$status <- as.factor(train$status)
levels(train$status)[1] <- 1
levels(train$status)[2] <- 0
model <- glm(status ~.,family = binomial(link = 'logit'), data = train)

#cross_valid_status <- cross_valid$status
#cross_valid[, c('status') := NULL]

# MT (Malta) -> IT (Italy), SV (Salvador) -> MX (Mexiko) 
#cross_valid[cross_valid$pais_residencia == 'MT']$pais_residencia <- 'IT'
#cross_valid[cross_valid$pais_residencia == 'SV']$pais_residencia <- 'MX'

#cross_valid[cross_valid$canal_entrada == 'KGN']$canal_entrada <- '_U'
#cross_valid[cross_valid$canal_entrada == 'KHR']$canal_entrada <- '_U'

#status_predict <- predict(model, newdata = cross_valid, type = 'response')

#cross_valid$status <- cross_valid_status
#cross_valid$status_predict <- ifelse (status_predict > 0.5, 'Added', 'Dropped')
#cross_valid_result <- nrow(cross_valid[cross_valid$status == cross_valid$status_predict]) / 
#    nrow(cross_valid)

#rm(cross_valid)
rm(train)
gc()

# prepare the test set similar to a train set
test <- test %>%
    gather(key = product, value = status, ind_ahor_fin_ult1:ind_recibo_ult1)
test <- data.table(test)
gc()

test$product <- as.factor(test$product)
test_ncodpers <- test$ncodpers
test[, c('status', 'ncodpers') := NULL]

status_predict <- predict(model, newdata = test, type = 'response')
