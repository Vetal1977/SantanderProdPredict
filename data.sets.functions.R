source('helper.functions.R')
library(zoo)

clean.and.split.train.df <- function(train) {
    # we have NA's for ind_nomina_ult1 & ind_nom_pens_ult1. set them to 0
    train$ind_nomina_ult1[is.na(train$ind_nomina_ult1)] <- 0
    train$ind_nom_pens_ult1[is.na(train$ind_nom_pens_ult1)] <- 0
    
    # unknown products ind_ahor_fin_ult1, ind_aval_fin_ult1 in test comparing to june 2015
    train <- train[, !(names(train) %in% c('ind_ahor_fin_ult1', 'ind_aval_fin_ult1'))]
    
    # clean and save only May 2015, June 2015, May 2016
    train.may.2015 <- train[train$fecha_dato == '2015-05-28',]
    train.june.2015 <- train[train$fecha_dato == '2015-06-28',]
    train.may.2016 <- train[train$fecha_dato == '2016-05-28',]
    
    train.lagged <- train[train$fecha_dato == '2015-01-28' |
                              train$fecha_dato == '2015-02-28' | 
                              train$fecha_dato == '2015-03-28' |
                              train$fecha_dato == '2015-04-28' |
                              train$fecha_dato == '2015-05-28',]
    
    test.lagged <- train[train$fecha_dato == '2016-01-28' |
                             train$fecha_dato == '2016-02-28' | 
                             train$fecha_dato == '2016-03-28' |
                             train$fecha_dato == '2016-04-28' |
                             train$fecha_dato == '2016-05-28',]
    
    train.may.2015 <- clean.data.in.df(train.may.2015)
    train.june.2015 <- clean.data.in.df(train.june.2015)
    train.may.2016 <- clean.data.in.df(train.may.2016)
    train.lagged <- clean.data.in.df(train.lagged)
    test.lagged <- clean.data.in.df(test.lagged)

    return(list(train.may.2015, train.june.2015, train.may.2016, train.lagged, test.lagged))
}

clean.test.df <- function(test, train.may.2016) {
    # products and ncodpers+products columns
    products <- grep("ind_+.*ult1$", names(train.may.2016))
    products_ncodpers <- c(2, products)
    
    # we need only ncodpers and products from May 2016 data frame
    train.may.2016 <- train.may.2016[, products_ncodpers]
    
    # clean merge with May 2016 products
    test <- clean.data.in.df(test)
    test <- merge(test, train.may.2016, by = c('ncodpers'), all.x = TRUE)
 
    return(test)   
}

prepare.train.df.for.boost <- function(train.may.2015, train.june.2015) {
    # filter June 2016 data - remove all rows by customers, where no products bought
    products <- grep("ind_+.*ult1$", names(train.june.2015))
    interesting <- rowSums(train.june.2015[, products])
    train.june.2015 <- train.june.2015[interesting > 0,]
    
    # prepare May 2015 data - we need only products and ncodpers to define products that were bought in June 2015
    products <- grep("ind_+.*ult1$", names(train.may.2015))
    products_ncodpers <- c(2, products)
    train.may.2015 <- train.may.2015[, products_ncodpers]
    
    # 'rotate' the May 2015 and June 2015 data set where each row corresponds to the tripple customer ID - product - status
    train.may.2015 <- train.may.2015 %>%
        gather(key = product, value = status, ind_cco_fin_ult1:ind_recibo_ult1)
    train.june.2015 <- train.june.2015 %>%
        gather(key = product, value = status, ind_cco_fin_ult1:ind_recibo_ult1)
    
    # remove unnecessary rows with products that was not bought
    train.june.2015 <- filter(train.june.2015, status > 0)
    
    # merge May 2015 and June 2016 data and, checking the bought/not-bought product status,
    # remove rows where no product was added
    train.june.2015 <- merge(
        train.june.2015, 
        train.may.2015, 
        by = c('ncodpers', 'product'),
        all.x = TRUE)
    train.june.2015[is.na(train.june.2015$status.y),]$status.y <- 0
    train.june.2015$added <- train.june.2015$status.x - train.june.2015$status.y
    train.june.2015 <- filter(train.june.2015, added > 0)
    train.june.2015$status.x <- NULL
    train.june.2015$status.y <- NULL
    train.june.2015$added <- NULL
    
    # convert product and status to factor
    train.june.2015$product <- as.factor(train.june.2015$product)
    
    return(train.june.2015)
}

make.prediction <- function(test, 
                            bst, 
                            train.june.2015) {
    # preparation
    to_predict <- prepare.predict.matrix(df = test)
    
    # predict and interpret the results
    pred <- predict(bst, newdata = to_predict)
    num.class <- length(levels(train.june.2015$product))
    pred <- matrix(pred, nrow=num.class, ncol=length(pred)/num.class)
    pred <- t(pred)
    colnames(pred) <- levels(train.june.2015$product)
    
    # exclude preditions for already bought products
    products <- grep("ind_+.*ult1$", names(test))
    prod_status <- test[, products]
    prod_status <- as.matrix(prod_status[, colnames(pred)])
    prod_status <- (1 - prod_status)
    pred <- prod_status * pred
    
    # put predictions to test data.frame
    test[, products] <- NULL
    test <- cbind(test, pred)
    
    return(test)
}

get.result.df <- function(test) {
    # 'rotate' test data
    test.rotated <- test %>%
        gather(key = product, value = prob, ind_cco_fin_ult1:ind_viv_fin_ult1)
    
    # remove products with probability <= 0
    test.rotated <- test.rotated[test.rotated$prob > 0,]
    
    # sort by ncodpers and probability
    test.rotated <- test.rotated[order(test.rotated$ncodpers, -test.rotated$prob),]
    
    # select 7 most probable products for customers
    result <- as.data.frame(
        test.rotated %>%
            group_by(ncodpers) %>%
            top_n(7)
    )
    
    return(result)
}

prepare.result.to.write <- function(result) {
    # prepare results to write
    result_write <- result %>% 
        group_by(ncodpers) %>% 
        summarise(added_products = paste(product, collapse = ' '))
    result_write <- as.data.frame(result_write)
    
    return(result_write)
}

make.lagged.set <- function(lagged.df, target.df, target.date) {
    lagged.df <- rbind(lagged.df, target.df)
    lagged.df <- lagged.df[order(lagged.df$ncodpers, lagged.df$fecha_dato),]
    lagged.df <- as.data.table(lagged.df)
    
    lagged.cols <- grep('ind_+.*ult1$', names(lagged.df), value = TRUE)
    lagged.cols <- c(lagged.cols, 'indrel_1mes', 'tiprel_1mes')
    lagged.cols.col.prev <- lagged.cols
    for (i in 1:5) {
        lagged.cols.lag <- paste('lag.col', lagged.cols, i, sep='.')
        lagged.df[, (lagged.cols.lag) := shift(.SD), 
                     by = ncodpers, 
                     .SDcols = lagged.cols.col.prev]
        lagged.cols.col.prev <- lagged.cols.lag
    }
    target.df.lagged <- as.data.frame(lagged.df[lagged.df$fecha_dato == target.date])
    target.df.lagged[is.na(target.df.lagged)] <- 0
    return(target.df.lagged)
}
