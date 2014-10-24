myPredictionModel <- function(testData)
{
    
    data <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
    testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
    data.nonNACols <- unlist(lapply(data, function(x) !any(is.na(x))), use.names=FALSE)
    data.nonNA <- data[, data.nonNACols]
    
    testing.nonNACols <- unlist(lapply(testing, function(x) !any(is.na(x))), use.names=FALSE)
    testing.nonNA <-  testing[, testing.nonNACols]
    
    data.nonNA.both <- ( data.nonNACols &  testing.nonNACols)
    data.nonNA <- data[, data.nonNA.both]
    testing.nonNA <-  testing[, data.nonNA.both]
    
    classeVar <- data.nonNA$classe
    data.nonNA <- data.frame(lapply(data.nonNA[,-60] , function(x) as.numeric(x)))
    data.nonNA$classe <- classeVar
    
    inTrain <- createDataPartition(data.nonNA$classe, p=.7, list=FALSE)
    data.training <- data.nonNA[inTrain, ]
    data.testing <- data.nonNA[-inTrain,]
    
    preProcess <- preProcess(data.training[,-60], method="pca", thresh=.99)
    data.pca <- predict( preProcess, data.training[,-60])
    
    model <- train(data.training$classe ~., method="knn", data=data.pca)
    data.pca.test <- predict( preProcess, data.testing[,-60])
    pred <- predict(model, data.pca.test)
    con <-  confusionMatrix(pred, data.testing$classe )
    
    
    testing.nonNA <- data.frame(lapply(testing.nonNA[,-60] , function(x) as.numeric(x)))
    testing.pca.test <- predict( preProcess, testing.nonNA[,-60])
    pred <- predict(model, testing.pca.test)
    as.character(pred)
    
}