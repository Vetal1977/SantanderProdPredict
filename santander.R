library(data.table)
library(rworldmap)
library(rpart)
library(ggplot2)

# select rows to analyze/clean
selectRow <- c('ncodpers', 'fecha_dato',
               'ind_empleado', 'pais_residencia', 'sexo', 'age', 
               'fecha_alta', 'ind_nuevo', 'antiguedad', 'indrel',
               'ult_fec_cli_1t', 'indrel_1mes', 'tiprel_1mes', 'conyuemp',
               'canal_entrada', 'ind_actividad_cliente', 'renta')

# read the train data from the file
train <- fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE, select = selectRow)
test <- fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE, select = selectRow)


# summary
summary(train) # seems, we have 27734 rows with age, ind_nuevo, antiguedad and some other

head(train[is.na(train$age)], 2)
tail(train[is.na(train$age)], 2)
train <- train[!is.na(train$age)]

country <- train[, .N, by = pais_residencia]
plotData <- joinCountryData2Map(dF = country, joinCode = 'ISO2', 
                                nameJoinColumn = 'pais_residencia', verbose=F)
mapCountryData(mapToPlot = plotData, nameColumnToPlot = 'N', catMethod = 'logFixedWidth',
               oceanCol = 'steelblue1',missingCountryCol = 'white',
               mapTitle = 'Number of customers by country',
               aspect = 'variable')

train <- train[train$age >= 18 & train$age <= 83]

# cannot use decision tree to predict the income since
# for NA's income there are new levels (for country, some dates)
#incomeFit <- rpart(renta ~ ind_empleado + sexo + age + 
#                       ind_nuevo + antiguedad + indrel + indrel_1mes +
#                       conyuemp + ind_actividad_cliente,
#                   data = train[!is.na(train$renta)],
#                   method = 'anova')
#train[is.na(train$renta)]$renta <- predict(incomeFit, train[is.na(train$renta),])

train[is.na(train$renta)]$renta <- median(train[!is.na(train$renta)]$renta)


ggplot(train, aes(x = ind_empleado, y = renta, fill=ind_empleado))+
    ggtitle('Income by employee index')+
    geom_boxplot(na.rm = TRUE) + scale_y_log10()+
    scale_fill_discrete(name = 'Employee index',
                        labels = c('Active', 'Ex employed', 'filial', 'Not employee', 'Pasive'))

