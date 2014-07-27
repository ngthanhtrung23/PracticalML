library(RCurl)

# Read training data
trainingFilename <- 'pml-training.csv'

if (!file.exists(trainingFilename)) {
    url <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv')
    trainData <- read.csv(text = url)
} else {
    trainData <- read.csv(trainingFilename, header=T, sep=',', skip=0, nrows=19623)
}

# Read testing data
testingFilename <- 'pml-testing.csv'

if (!file.exists(testingFilename)) {
    url <- getURL('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv')
    testData <- read.csv(text = url)
} else {
    testData <- read.csv(testingFilename, header=T, sep=',', skip=0, nrows=21)
}

# Convert data to all numeric
for (col in 1:dim(trainData)[2]) {
    if (!is.null(levels(trainData[,col]))) {
        trainData[,col] = as.numeric(trainData[,col])
        testData[,col] = as.numeric(testData[,col])
    }
}

# Remove near zeros columns
nsv <- nearZeroVar(trainData, saveMetrics=T)
trainData <- data.frame(trainData[,!nsv$nzv])
testData <- data.frame(testData[,!nsv$nzv])

# Load libraries & set seed
library(caret)
set.seed(1234)

# Remove columns with NA
goodColumns <- colSums(is.na(trainData)) == 0
trainData <- trainData[, goodColumns]
testData <- testData[, goodColumns]

# Transform data using center, scaling & pca
preProc <- preProcess(trainData, method=c("center", "scale", "pca"))

problem_id <- testData$problem_id
names(testData)[names(testData) == 'problem_id'] <- 'classe'

trainTransformed <- predict(preProc, trainData)
testTransformed <- predict(preProc, testData)

trainTransformed$classe <- trainData$classe
testTransformed$problem_id <- problem_id

# Split data for training & CV
inTrain <- createDataPartition(y=trainTransformed$classe, p = 0.3, list = F)
training <- trainTransformed[inTrain,]
cv <- trainTransformed[-inTrain,]
inTest <- createDataPartition(y=testTransformed$problem_id, p = 1, list = F)
dim(training)
test <- testTransformed[inTest,]

# Build model & predict
modelFit <- train(classe ~ ., data = training, method="lm")
predictions <- predict(modelFit, newdata=test)
predictions
