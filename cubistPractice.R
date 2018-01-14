###cubist demo for trying out new function
###use cars mpg dataset: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/
set.seed(50)
require(Cubist)
require(caret)

cars.mpg <- read.csv("C:/Users/johnb/Documents/Code Projects/Github/carsmpg.csv")
cars.mpg$carname <- gsub("\"", "", cars.mpg$carname)

head(cars.mpg)


cars.mpg$carname <- NULL

###take out mpg, carname
predictors <- colnames(cars.mpg)[-1]
predictors <- predictors[-length(predictors)]

###set correct data types for variables-> all as characters when read from csv
cars.mpg$mpg <- as.numeric(cars.mpg$mpg)
cars.mpg$cylinders <- as.factor(cars.mpg$cylinders)
cars.mpg$displacement <- as.numeric(cars.mpg$displacement)
cars.mpg$horsepower <- as.numeric(cars.mpg$horsepower)
cars.mpg$weight <- as.numeric(cars.mpg$weight)
cars.mpg$acceleration <- as.numeric(cars.mpg$acceleration)
cars.mpg$modelyear <- as.factor(cars.mpg$modelyear)
cars.mpg$origin <- as.factor(cars.mpg$origin)


###separate into train/test
intrain <- createDataPartition(cars.mpg$mpg, times = 1, p = 0.65)$Resample1
intest <- c(1:nrow(cars.mpg))[-intrain]

traindata <- cars.mpg[intrain,]
testdata <- cars.mpg[intest,]

model <- cubist(x = traindata[,predictors], y = traindata$mpg, committees = 5)


##make predictions on test data
testdata$pred <- predict(model, newdata = testdata[,predictors], neighbors = 1)

###plort results
gg <- ggplot(testdata) + geom_point(aes(x=c(1:dim(testdata)[1]), y = mpg), col = "purple") + geom_line(aes(x=c(1:dim(testdata)[1]), y = pred), col = "red")
plot(gg)
