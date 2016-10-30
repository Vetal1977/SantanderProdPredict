library(data.table)
library(rworldmap)
library(rpart)
library(ggplot2)

# read the train data from the file
train <- fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)
test <- fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)

# summary
#summary(train) 
#head(train[is.na(train$age)], 2)
#tail(train[is.na(train$age)], 2)

# intersection between train and test sets
#trainUniquePersCode <- unique(train$ncodpers)
#testUniquePersCode <- unique(test$ncodpers)
#personIntersection <- intersect(trainUniquePersCode, testUniquePersCode)

# seems, we have 27734 useless rows with age, ind_nuevo, antiguedad and some other set to NA
# while other features set to ''. just remove them
train <- train[!is.na(train$age)]

# we have NA's for ind_nomina_ult1 & ind_nom_pens_ult1. set them to 0
train[is.na(train$ind_nomina_ult1)]$ind_nomina_ult1 <- 0
train[is.na(train$ind_nom_pens_ult1)]$ind_nom_pens_ult1 <- 0

# limit ages
ageMedian <- median(train[train$age >= 18 & train$age <= 90]$age)
train[train$age < 18 & train$age > 90]$age <- ageMedian

# NA's for tipodom. set it to 1
train[is.na(train$tipodom)]$tipodom <- 1

# we have too many NA's for income
# check their distribution by countries
#countryWithoutIncome <- train[is.na(train$renta), .N, by = pais_residencia]
#plotData <- joinCountryData2Map(dF = countryWithoutIncome, joinCode = 'ISO2', 
#                                nameJoinColumn = 'pais_residencia', verbose=F)
#mapCountryData(mapToPlot = plotData, nameColumnToPlot = 'N', catMethod = 'logFixedWidth',
#               oceanCol = 'steelblue1',missingCountryCol = 'white',
#               mapTitle = 'Number of customers by country',
#               aspect = 'variable')

# checking the income statistic in our train and test sets
# we see that
# 1. it is almost the same for both sets
# 2. we have statistic info mainly for Spain
# 3. foreign account owners have much higher income
#summary(test[!is.na(test$renta)]$renta)
#summary(test[!is.na(test$renta) & test$pais_residencia == 'ES']$renta)
#summary(test[!is.na(test$renta) & test$pais_residencia != 'ES']$renta)
#summary(train[!is.na(train$renta)]$renta)
#summary(train[!is.na(train$renta) & train$pais_residencia == 'ES']$renta)
#summary(train[!is.na(train$renta) & train$pais_residencia != 'ES']$renta)

# at the first step we calculate medians separately for Spain people and foreigners
medianIncomeSpain <- median(train[!is.na(train$renta) & train$pais_residencia == 'ES']$renta)
train[is.na(train$renta) & train$pais_residencia == 'ES']$renta <- medianIncomeSpain

medianIncomeForeign <- median(train[!is.na(train$renta) & train$pais_residencia != 'ES']$renta)
train[is.na(train$renta) & train$pais_residencia != 'ES']$renta <- medianIncomeForeign

# we have NA's for province code - mostly for foreign owners. just set it to 0
train[is.na(train$cod_prov)]$cod_prov <- 0

# here we got rid of NA's
# we have 17 unique train dates (fecha_dato) and one test date, so it's clean
uniqueTrainDates <- unique(train$fecha_dato)
uniqueTestDates <- unique(test$fecha_dato)

# 1. inactive customers cannot buy new products (?)
test <- test[test$ind_actividad_cliente != 0]

# 2. limit ages for test set too 
test[test$age < 18 & test$age > 90]$age <- ageMedian

# 3. set income for NA's for test set too
test[is.na(test$renta) & test$pais_residencia == 'ES']$renta <- medianIncomeSpain
test[is.na(test$renta) & test$pais_residencia != 'ES']$renta <- medianIncomeForeign

# 4. set province code to 0 for NA's for test set too
test[is.na(test$cod_prov)]$cod_prov <- 0

# 5. remove indrel_1mes NA's
test[is.na(test$indrel_1mes)]$indrel_1mes <- 
    train[train$ncodpers %in% test[is.na(test$indrel_1mes)]$ncodpers]$indrel_1mes

# TODO: the products may change from 0 to 1 and vice versa each month
