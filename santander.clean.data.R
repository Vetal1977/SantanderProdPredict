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

write.table(train, 'train_status_change.csv', quote = FALSE, row.names = FALSE, sep = ';')

