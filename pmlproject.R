library(caret)
trg <- read.csv(file = "~/Downloads/pml-training.csv", header = TRUE)
val <- read.csv(file = "~/Downloads/pml-testing.csv", header = TRUE)
set.seed(101)

# Isolate variables with acceleration information, remove variance variables which are all NA
varList <- grep("accel", colnames(trg))
trg <- data.frame(trg[, varList], classe = trg$classe)
val <- data.frame(val[, varList])
navar <- grep("^var", colnames(trg))
trg <- data.frame(trg[, -navar])
val <- data.frame(val[, -navar])

# Further split trg into training set and test set. Actual test set used as validation set instead
inTrain <- createDataPartition(trg$classe, p = 0.9, list = FALSE)
training <- trg[inTrain, ]
testing <- trg[-inTrain, ]

# Preliminary data plots of predictors
# featurePlot(x = training[, -16], y = training$classe, plot = "pairs")
# nzv <- nearZeroVar(trg)

# Set train control
tr_ctrl <- trainControl(method = "cv", number = 10)

# Train simple classification tree
dt_model <- train(classe ~ ., data = training, method = "rpart", trControl = tr_ctrl)
confusionMatrix(testing$classe, predict(dt_model, testing))

# Train random forest
rf_model <- train(classe ~ ., data = training, method = "rf", trControl = tr_ctrl)
confusionMatrix(testing$classe, predict(rf_model, testing))

# Train boosting model
gbm_model <- train(classe ~ ., data = training, method = "gbm", trControl = tr_ctrl)
confusionMatrix(testing$classe, predict(gbm_model, testing))