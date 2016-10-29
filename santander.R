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
summary(train) 
head(train[is.na(train$age)], 2)
tail(train[is.na(train$age)], 2)

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
countryWithoutIncome <- train[is.na(train$renta), .N, by = pais_residencia]
plotData <- joinCountryData2Map(dF = countryWithoutIncome, joinCode = 'ISO2', 
                                nameJoinColumn = 'pais_residencia', verbose=F)
mapCountryData(mapToPlot = plotData, nameColumnToPlot = 'N', catMethod = 'logFixedWidth',
               oceanCol = 'steelblue1',missingCountryCol = 'white',
               mapTitle = 'Number of customers by country',
               aspect = 'variable')

# checking the income statistic in our train and test sets
# we see that
# 1. it is almost the same for both sets
# 2. we have statistic info mainly for Spain
# 3. foreign account owners have much higher income
summary(test[!is.na(test$renta)]$renta)
summary(test[!is.na(test$renta) & test$pais_residencia == 'ES']$renta)
summary(test[!is.na(test$renta) & test$pais_residencia != 'ES']$renta)
summary(train[!is.na(train$renta)]$renta)
summary(train[!is.na(train$renta) & train$pais_residencia == 'ES']$renta)
summary(train[!is.na(train$renta) & train$pais_residencia != 'ES']$renta)

# at the first step we calculate medians separately for Spain people and foreigners
medianIncomeSpain <- median(train[!is.na(train$renta) & train$pais_residencia == 'ES']$renta)
medianIncomeForeign <- median(train[!is.na(train$renta) & train$pais_residencia != 'ES']$renta)

train[is.na(train$renta) & train$pais_residencia == 'ES']$renta <- medianIncomeSpain
train[is.na(train$renta) & train$pais_residencia != 'ES']$renta <- medianIncomeForeign

# we have NA's for province code - mostly for foreign owners. just set it to 0
train[is.na(train$cod_prov)]$cod_prov <- 0

# here we got rid of NA's and ready to train the model
