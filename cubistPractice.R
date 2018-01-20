###cubist demo for trying out new function
###use cars mpg dataset: https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/
set.seed(50)
require(Cubist)
require(caret)

cars.mpg <- read.csv("C:/Users/johnb/Documents/Code Projects/Github/carsmpg.csv", stringsAsFactors = FALSE)
cars.mpg$carname <- gsub("\"", "", cars.mpg$carname)

head(cars.mpg)

###PREPROCESSING: make sure data is in correct data types for model
###there's certain parameters that we'd like to have as factors, rather than numeric
###in addition, there's a few missing values listed as "?" under cars.mpg$horsepower.

rows.to.delete <- which(cars.mpg$horsepower == "?")
tot.rows <- nrow(cars.mpg)

print(paste("We are looking to get rid of", length(rows.to.delete), "rows of", tot.rows, "total rows."))

###6 << 398. we can probably get rid of these without significantly effecting the analysis

cars.mpg <- cars.mpg[-rows.to.delete,]
###lapply gives the results we want in order to check the classes
lapply(cars.mpg, class)

###at this point, it makes sense to have horsepower as a numeric, cylinders as a factor, and origin as a factor
cars.mpg[["cylinders"]] <- as.factor(cars.mpg[["cylinders"]])
cars.mpg[["origin"]] <- as.factor(cars.mpg[["origin"]])
cars.mpg[["horsepower"]] <- as.numeric(cars.mpg[["horsepower"]])
###car name doesn't really make sense to have as a predictor, unless we break the strings into discrete manufacturers
###given that this is an example, I just won't bother with it
cars.mpg$carname <- NULL

lapply(cars.mpg, class)
###take out mpg for predictors
predictors <- colnames(cars.mpg)[-1]

###separate into train/test
intrain <- createDataPartition(cars.mpg$mpg, times = 1, p = 0.65)$Resample1
intest <- c(1:nrow(cars.mpg))[-intrain]

traindata <- cars.mpg[intrain,]
testdata <- cars.mpg[intest,]

model <- cubist(x = traindata[,predictors], y = traindata$mpg, committees =10)


##make predictions on test data
testdata$pred <- predict(model, newdata = testdata[,predictors], neighbors = 1)

###plort results
gg <- ggplot(testdata) + geom_point(aes(x=c(1:dim(testdata)[1]), y = mpg), col = "purple") + geom_line(aes(x=c(1:dim(testdata)[1]), y = pred), col = "red")
plot(gg)

