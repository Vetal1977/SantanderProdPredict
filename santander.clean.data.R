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
        if (length(x) < 17) {
            diffs <- c(x[1], diffs)
        }
        else {
            diffs <- c(0, diffs)
        }
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
ageMedian <- median(combine[combine$age >= 18 & combine$age <= 90]$age, na.rm = TRUE)
combine[is.na(combine$age)]$age <- ageMedian
combine[combine$age < 18 | combine$age > 90]$age <- ageMedian

# remove tipodom, conyuemp and
# remove ult_fec_cli_1t - last date as primary customer (if he isn't at the end of the month)
# we have code and name columns for province - remove duplicated information, province code
combine[, c('conyuemp', 'tipodom', 'ult_fec_cli_1t', 'cod_prov') := NULL]

# and replace empty province code with _U
combine$nomprov[combine$nomprov == ''] <- '_U'
combine[combine$pais_residencia == '']$pais_residencia <- '_U'
gc()

# calculate incomes for missing values
new.incomes <- combine %>%
    select(nomprov) %>%
    merge(combine %>%
              group_by(nomprov) %>%
              summarise(med.income = median(renta, na.rm=TRUE)),
          by="nomprov") %>%
    select(nomprov, med.income) %>%
    arrange(nomprov)

combine <- arrange(combine, nomprov)
combine$renta[is.na(combine$renta)] <- new.incomes$med.income[is.na(combine$renta)]
rm(new.incomes)

combine$renta[is.na(combine$renta)] <- median(combine$renta, na.rm = TRUE)
combine <- arrange(combine, fecha_dato)
gc()

# check for empty strings
#char.cols <- names(combine)[sapply(combine, is.character)]
#for (name in char.cols){
#    print(sprintf("Unique values for %s:", name))
#    print(unique(combine[[name]]))
#    cat('\n')
#}

# replace empty strings and NA's
combine$ind_nuevo[is.na(combine$ind_nuevo)] <- 1
combine$antiguedad[is.na(combine$antiguedad)] <- min(combine$antiguedad, na.rm=TRUE)
combine$antiguedad[combine$antiguedad < 0] <- 0
combine$indrel[is.na(combine$indrel)] <- 1
combine$ind_actividad_cliente[is.na(combine$ind_actividad_cliente)] <- 1
combine$indrel_1mes[is.na(combine$indrel_1mes)] <- '1'

combine$tiprel_1mes[combine$tiprel_1mes == ''] <- 'A'
combine$indrel_1mes[combine$indrel_1mes == ''] <- '1'
combine$indrel_1mes[combine$indrel_1mes == 'P'] <- '5'
combine$indrel_1mes <- as.integer(combine$indrel_1mes)
combine$indrel_1mes[combine$indrel_1mes > 5] <- 1
combine$indrel_1mes[is.na(combine$indrel_1mes)] <- 1
combine$tiprel_1mes[combine$tiprel_1mes == ''] <- 'A'
combine$sexo[combine$sexo == ''] <- '_U'
combine$canal_entrada[combine$canal_entrada == ''] <- '_U'
combine$segmento[combine$segmento == ''] <- '_U'
combine$ind_empleado[combine$ind_empleado == ''] <- '_U'
combine$indresi[combine$indresi == ''] <- '_U'
combine$indext[combine$indext == ''] <- '_U'
combine$indfall[combine$indfall == ''] <- 'N'
gc()

# make dates and factors
combine$fecha_dato <- as.Date(combine$fecha_dato)
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

# replace NA's for feche_alta here
median.fecha.alta <- median(as.Date(combine$fecha_alta[combine$fecha_alta != '']))
combine$fecha_alta[combine$fecha_alta == ''] <- as.character(median.fecha.alta)
combine$fecha_alta <- as.Date(combine$fecha_alta)

# split combine data set into train and test again
train <- combine[combine$fecha_dato != '2016-06-28',]
gc()
test <- combine[combine$fecha_dato == '2016-06-28',]
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
train[, products] <- lapply(train[, products], function(x) as.integer(round(x)))
train[, products] <- lapply(train[, products], 
                            function(x) {
                                return(ave(x, train$ncodpers, FUN = product.status.change))
                            }
)

write.table(train, 'train_status_change.csv', quote = FALSE, row.names = FALSE, sep = ';')

