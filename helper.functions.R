library(data.table)
library(dplyr)
library(tidyr)
library(reshape2)
library(xgboost)
library(Matrix)

clean.data.in.df <- function(df) {
    # limit ages
    min.age <- 18.0
    max.age <- 90.0
    range.age <- (max.age - min.age)
    df$age[df$age < min.age] <- min.age
    df$age[df$age > max.age] <- max.age
    df$age[is.na(df$age)] <- median(df$age, na.rm = TRUE)
    df$age <- round((df$age - min.age) / range.age, 6)
    
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
    
    min.income <- min(df$renta)
    max.income <- max(df$renta)
    range.income <- max.income - min.income
    df$renta[df$renta < min.income] <- min.income
    df$renta[df$renta > max.income] <- max.income
    df$renta <- round((df$renta - min.income) / range.income, 6)
    
    # check for empty strings
    #char.cols <- names(df)[sapply(df, is.character)]
    #for (name in char.cols){
    #    print(sprintf("Unique values for %s:", name))
    #    print(unique(df[[name]]))
    #    cat('\n')
    #}
    
    # seniority
    min.seniority <- 0
    max.seniority <- 256
    range.seniority <- max.seniority - min.seniority
    df$antiguedad[is.na(df$antiguedad)] <- min.seniority
    df$antiguedad[df$antiguedad < min.seniority] <- min.seniority
    df$antiguedad[df$antiguedad > max.seniority] <- max.seniority
    df$antiguedad <- round((df$antiguedad - min.seniority) / range.seniority, 6)
    
    # replace empty strings and NA's
    df$ind_nuevo[is.na(df$ind_nuevo)] <- 1
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
    df$indfall <- as.factor(df$indfall)
    gc()
    
    # replace NA's for feche_alta here
    median.fecha.alta <- median(as.Date(df$fecha_alta[df$fecha_alta != '']))
    df$fecha_alta[df$fecha_alta == ''] <- as.character(median.fecha.alta)
    df$fecha_alta <- as.Date(df$fecha_alta)
    
    return(df)
}

prepare.predict.matrix <- function(df) {
    train.columns <- colnames(df)
    feature.columns <- grep('lag.col.*', train.columns)
    feature.columns <- c(feature.columns, 
                         which(train.columns %in% c(
                             'age', 'renta', 'sexo', 'ind_nuevo',
                             'segmento', 'ind_actividad_cliente',
                             'canal_entrada')))
    df$sexo <- as.numeric(df$sexo)
    df$sexo <- scale.feature(df$sexo)
    
    df$ind_nuevo <- as.numeric(df$ind_nuevo)
    df$ind_nuevo <- scale.feature(df$ind_nuevo)
    
    df$segmento <- as.numeric(df$segmento)
    df$segmento <- scale.feature(df$segmento)
    
    df$ind_actividad_cliente <- as.numeric(df$ind_actividad_cliente)
    df$ind_actividad_cliente <- scale.feature(df$ind_actividad_cliente)
    
    df$canal_entrada <- as.numeric(df$canal_entrada)
    df$canal_entrada <- scale.feature(df$canal_entrada)
    
    predict.matrix <- as.matrix(df[, feature.columns])
    return(predict.matrix)
}

scale.feature <- function(feature.vec) {
    feature.min <- min(feature.vec)
    feature.max <- max(feature.vec)
    feature.range <- feature.max - feature.min
    feature.scale <- round((feature.vec - feature.min) / feature.range, 6)
    return(feature.scale)
}
