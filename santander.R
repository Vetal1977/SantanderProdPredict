library(data.table)

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

# NA's for tipodom. set it to 1
combine[is.na(combine$tipodom)]$tipodom <- 1

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

combine[combine$conyuemp == '']$conyuemp <- '0'
combine$conyuemp <- as.integer(combine$conyuemp)
gc()

# split combine data set into train and test again
train <- combine[combine$fecha_dato != '2016-06-28']
gc()
test <- combine[combine$fecha_dato == '2016-06-28']
rm(combine)
gc()

# clean up products in the thrain data set
#products <- grep("ind_+.*ult.*", names(train))
#train[, products] <- lapply(train[, products], FUN = function(x) { as.integer(round(x)) })
#gc()






# indrel, age, segmento, canal_entrada, age, antiguedad may change from time to time!
trainUniquePers <- train[, by = list(ncodpers, ind_empleado, pais_residencia, sexo, age,
                                    indrel, canal_entrada, cod_prov, renta, segmento), 
               lapply(.SD, max),
               .SDcols = c('age', 'antiguedad',
                           'ind_ahor_fin_ult1', 'ind_aval_fin_ult1', 'ind_cco_fin_ult1',
                           'ind_cder_fin_ult1', 'ind_cno_fin_ult1', 'ind_ctju_fin_ult1',
                           'ind_ctma_fin_ult1', 'ind_ctop_fin_ult1', 'ind_ctpp_fin_ult1',
                           'ind_deco_fin_ult1', 'ind_deme_fin_ult1', 'ind_dela_fin_ult1',
                           'ind_ecue_fin_ult1', 'ind_fond_fin_ult1', 'ind_hip_fin_ult1',
                           'ind_plan_fin_ult1', 'ind_pres_fin_ult1', 'ind_reca_fin_ult1',
                           'ind_tjcr_fin_ult1', 'ind_valo_fin_ult1', 'ind_viv_fin_ult1',
                           'ind_nomina_ult1', 'ind_nom_pens_ult1', 'ind_recibo_ult1')]

# clean test data

# 2. limit ages for test set too 
test[test$age < 18 & test$age > 90]$age <- ageMedian

# 3. set income for NA's for test set too
test[is.na(test$renta) & test$pais_residencia == 'ES']$renta <- medianIncomeSpain
test[is.na(test$renta) & test$pais_residencia != 'ES']$renta <- medianIncomeForeign

# 4. remove cod_prov
test[is.na(test$cod_prov)]$cod_prov <- 0

# 5. remove indrel_1mes NA's
test[is.na(test$indrel_1mes)]$indrel_1mes <- 
    train[train$ncodpers %in% test[is.na(test$indrel_1mes)]$ncodpers]$indrel_1mes

# 6. deceased cannot buy new products
test <- test[test$indfall != 'S']

# TODO: the products may change from 0 to 1 and vice versa each month

# - Find customer clusters
trainCluster <- train[, c('ncodpers', 'age', 'sexo', 'cod_prov', 'renta', 'segmento'), with = FALSE]
trainCluster <- trainCluster[!duplicated(trainCluster$ncodpers)]
trainCluster$ncodpers <- NULL
trainCluster$sexo <- as.factor(trainCluster$sexo)
trainCluster$cod_prov <- as.factor(trainCluster$cod_prov)
trainCluster$segmento <- as.factor(trainCluster$segmento)

trainClusterMatrix <- model.matrix(~.+0, data = trainCluster)
trainClusterMatrix$sexo <- NULL
trainClusterMatrix$cod_prov <- NULL
trainClusterMatrix$segmento <- NULL

cluster <- kmeans(trainClusterMatrix, 5, nstart = 20)

# - For each cluster make a forecasting of products
