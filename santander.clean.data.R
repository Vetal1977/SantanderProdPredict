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

clean.data.in.df <- function(df) {
    # limit ages
    ageMedian <- median(df$age[df$age >= 18 & df$age <= 100], na.rm = TRUE)
    df$age[is.na(df$age)] <- ageMedian
    df$age[df$age < 18 | df$age > 100] <- ageMedian
    
    # remove tipodom, conyuemp and
    # remove ult_fec_cli_1t - last date as primary customer (if he isn't at the end of the month)
    # we have code and name columns for province - remove duplicated information, province code
    df <- df[, !(names(df) %in% 
                     c('conyuemp', 'tipodom', 'ult_fec_cli_1t', 'cod_prov'))]
        
    # and replace empty province code with _U
    df$nomprov[df$nomprov == ''] <- '_U'
    df$pais_residencia[df$pais_residencia == ''] <- '_U'
    gc()
    
    # calculate incomes for missing values
    new.incomes <- df %>%
        select(nomprov) %>%
        merge(df %>%
                  group_by(nomprov) %>%
                  summarise(med.income = median(renta, na.rm=TRUE)),
              by="nomprov") %>%
        select(nomprov, med.income) %>%
        arrange(nomprov)
    
    df <- arrange(df, nomprov)
    df$renta[is.na(df$renta)] <- new.incomes$med.income[is.na(df$renta)]
    rm(new.incomes)
    
    df$renta[is.na(df$renta)] <- median(df$renta, na.rm = TRUE)
    df <- arrange(df, fecha_dato)
    gc()
    
    # check for empty strings
    #char.cols <- names(df)[sapply(df, is.character)]
    #for (name in char.cols){
    #    print(sprintf("Unique values for %s:", name))
    #    print(unique(df[[name]]))
    #    cat('\n')
    #}
    
    # replace empty strings and NA's
    df$ind_nuevo[is.na(df$ind_nuevo)] <- 1
    df$antiguedad[is.na(df$antiguedad)] <- min(df$antiguedad, na.rm=TRUE)
    df$antiguedad[df$antiguedad < 0] <- 0
    df$indrel[is.na(df$indrel)] <- 1
    df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- 1
    df$indrel_1mes[is.na(df$indrel_1mes)] <- '1'
    
    df$tiprel_1mes[df$tiprel_1mes == ''] <- 'A'
    df$indrel_1mes[df$indrel_1mes == ''] <- '1'
    df$indrel_1mes[df$indrel_1mes == 'P'] <- '5'
    df$indrel_1mes <- as.integer(df$indrel_1mes)
    df$indrel_1mes[df$indrel_1mes > 5] <- 1
    df$indrel_1mes[is.na(df$indrel_1mes)] <- 1
    df$tiprel_1mes[df$tiprel_1mes == ''] <- 'A'
    df$sexo[df$sexo == ''] <- '_U'
    df$canal_entrada[df$canal_entrada == ''] <- '_U'
    df$segmento[df$segmento == ''] <- '_U'
    df$ind_empleado[df$ind_empleado == ''] <- '_U'
    df$indresi[df$indresi == ''] <- '_U'
    df$indext[df$indext == ''] <- '_U'
    df$indfall[df$indfall == ''] <- 'N'
    gc()
    
    # make dates and factors
    df$fecha_dato <- as.Date(df$fecha_dato)
    df$ind_empleado <- as.factor(df$ind_empleado)
    df$pais_residencia <- as.factor(df$pais_residencia)
    df$sexo <- as.factor(df$sexo)
    df$ind_nuevo <- as.factor(df$ind_nuevo)
    df$indrel <- as.factor(df$indrel)
    df$indrel_1mes <- as.factor(df$indrel_1mes)
    df$tiprel_1mes <- as.factor(df$tiprel_1mes)
    df$indresi <- as.factor(df$indresi)
    df$indext <- as.factor(df$indext)
    df$canal_entrada <- as.factor(df$canal_entrada)
    df$nomprov <- as.factor(df$nomprov)
    df$ind_actividad_cliente <- as.factor(df$ind_actividad_cliente)
    df$segmento <- as.factor(df$segmento)
    gc()
    
    # replace NA's for feche_alta here
    median.fecha.alta <- median(as.Date(df$fecha_alta[df$fecha_alta != '']))
    df$fecha_alta[df$fecha_alta == ''] <- as.character(median.fecha.alta)
    df$fecha_alta <- as.Date(df$fecha_alta)
    
    return(df)
}

# read the train data from the file
train <- as.data.frame(
    fread('train_ver2.csv', sep = ',', na.strings = 'NA', 
               stringsAsFactors = FALSE)
)

# we have NA's for ind_nomina_ult1 & ind_nom_pens_ult1. set them to 0
train$ind_nomina_ult1[is.na(train$ind_nomina_ult1)] <- 0
train$ind_nom_pens_ult1[is.na(train$ind_nom_pens_ult1)] <- 0

# unknown products ind_ahor_fin_ult1, ind_aval_fin_ult1 in test comparing to june 2015
train <- train[, !(names(train) %in% c('ind_ahor_fin_ult1', 'ind_aval_fin_ult1'))]

# clean and save only May 2015, June 2015, May 2016
train.may.2015 <- train[train$fecha_dato == '2015-05-28',]
train.june.2015 <- train[train$fecha_dato == '2015-06-28',]
train.may.2016 <- train[train$fecha_dato == '2016-05-28',]

train.may.2015 <- clean.data.in.df(train.may.2015)
train.june.2015 <- clean.data.in.df(train.june.2015)
train.may.2016 <- clean.data.in.df(train.may.2016)

write.table(train.may.2015, 'train_may_2015.csv', quote = FALSE, row.names = FALSE, sep = ';')
write.table(train.june.2015, 'train_june_2015.csv', quote = FALSE, row.names = FALSE, sep = ';')
write.table(train.may.2016, 'train_may_2016.csv', quote = FALSE, row.names = FALSE, sep = ';')

# read test data from the file and combine them with train data for cleaning
test <- as.data.frame(
    fread('test_ver2.csv', sep = ',', na.strings = 'NA', 
              stringsAsFactors = FALSE)
)

# clean and save
test <- clean.data.in.df(test)
write.table(test, 'test_clean.csv', quote = FALSE, row.names = FALSE, sep = ';')
