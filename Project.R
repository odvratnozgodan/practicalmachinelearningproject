if (!require("dplyr")) install.packages("dplyr")
if (!require("doParallel")) install.packages("doParallel")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("caret")) install.packages("caret")
if (!require("doMC")) install.packages("doMC")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(doMC))

if(!file.exists("./pml-training.csv")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(fileUrl, destfile="./pml-training.csv", method="curl")  
}

if(!file.exists("./pml-testing.csv")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(fileUrl, destfile="./pml-testing.csv", method="curl")  
}

training_data <- read.csv("./pml-training.csv", na.strings = c("#DIV/0!", "", "NA"))
submission_data <- read.csv("./pml-testing.csv", na.strings = c("#DIV/0!", "", "NA"))

# Remove the new_window because it's sparse and some features have values only on that observation
training_data <- training_data %>% filter(new_window == "no")

# This feature has no variablility so we will remove it
training_data$new_window <- NULL
submission_data$new_window <- NULL

# Removing columns with all values NA
training_data <- training_data[colSums(!is.na(training_data)) > 0]
submission_data <- submission_data[colSums(!is.na(submission_data)) > 0]

# Probably should remove timestamps, as were not going to model the data as poisson or time-series
training_data$X <- NULL
submission_data$X <- NULL
training_data$cvtd_timestamp <- NULL
submission_data$cvtd_timestamp <- NULL
training_data$raw_timestamp_part_1 <- NULL
submission_data$raw_timestamp_part_1 <- NULL
training_data$raw_timestamp_part_2 <- NULL
submission_data$raw_timestamp_part_2 <- NULL



# Check to see which columns in the testing dataset are NOT present in the training_data dataset
test_extras <- colnames(submission_data)[which(colnames(submission_data) %in% colnames(training_data)==FALSE)]
submission_data <- submission_data[ , !(names(submission_data) %in% test_extras)]

train_extras <- colnames(training_data)[which(colnames(training_data) %in% colnames(submission_data)==FALSE)]
training_data <- training_data[ , !(names(training_data) %in% train_extras[-which(train_extras=="classe")])]

##############################################
set.seed(33833)

inTrain = createDataPartition(training_data$classe, p = 0.7, list = FALSE)
training <- training_data[inTrain,]
validation_data <- training_data[-inTrain,]
inValidation = createDataPartition(validation_data$classe, p = 0.5, list = FALSE)
validation <- validation_data[inValidation,]
testing <- validation_data[-inValidation,]

registerDoMC(cores = detectCores())

tControl <- trainControl(method='cv', number=3, returnResamp='none')

methods <- list("rpart", "gbm", "rf", "rpart", "gbm", "rf")
preprocess <- list(NULL, NULL, NULL, c("center", "scale"), c("center", "scale"), c("center", "scale"))
accuracies <- data.frame(methods=matrix(unlist(methods), nrow=length(methods), byrow=T), preprocess=matrix(), accuracy=matrix())
if(!is.null(unlist(preprocess))){
    accuracies$preprocess <- lapply(preprocess, paste)    
}

pred_validation <- data.frame(model1=validation$classe)
pred_test <- data.frame(model1=validation$classe[1:nrow(testing)])
pred_submission <- data.frame(model1=validation$classe[1:nrow(submission_data)])

for (i in 1:length(methods)) {
    if(methods[[i]]=="gbm"){
        model <- train(classe~., training, method=methods[[i]], preProcess=preprocess[[i]], trControl=tControl, verbose=FALSE)        
    }else{
        model <- train(classe~., training, method=methods[[i]], preProcess=preprocess[[i]], trControl=tControl)
    }
    pred_validation[paste("model", i, sep = "")] <- predict(model, newdata = validation)
    pred_test[paste("model", i, sep = "")] <- predict(model, newdata = testing)
    pred_submission[paste("model", i, sep = "")] <- predict(model, newdata = submission_data)
    accuracies$accuracy[i] <- confusionMatrix(validation$classe, pred_validation[[paste("model", i, sep = "")]])$overall[1]
}

accuracies

bind_cols(validation, pred_validation)
bind_cols(testing, pred_test)
bind_cols(submission_data, pred_submission)

pred_validation$classe <- validation$classe
pred_test$classe <- testing$classe
pred_submission$classe <- submission_data$classe

# Fit a model that combines all models
#train a new model on the predictions
fit_ensemble <- train(classe~., data=pred_validation, method='gbm', trControl=tControl, verbose=FALSE)
pred_ensamble <- predict(fit_ensemble,pred_test)
pred_ensamble_submission <- predict(fit_ensemble,pred_submission)

accuracy <- confusionMatrix(pred_test$classe, pred_ensamble)$overall[1]

rmse <- RMSE(as.numeric(pred_ensamble), as.numeric(pred_test$classe))

pred_ensamble_submission

trainModels <- function(){
        binarizedTrain <- training %>% mutate(classe=ifelse(classe=="A", 1, 0))
        binarizedValidate <- validation %>% mutate(classe=ifelse(classe=="A", 1, 0))
    
        model1 <- train(classe~., binarizedTrain, method="rpart")
        pred_validation1 <- predict(model1, newdata = binarizedValidate)
        pred_submission1 <- predict(model1, newdata = testing)
        
        print(confusionMatrix(validation$classe, pred_validation1)$overall[1])
        
        model2 <- train(classe~., training, method="rpart", preProcess=c("pca"))
        pred_validation2 <- predict(model2, newdata = validation)
        pred_submission2 <- predict(model2, newdata = testing)
        
        model3 <- train(classe~., training_proc, method="gbm")
        pred_validation3 <- predict(model3, newdata = validation)
        pred_submission3 <- predict(model3, newdata = testing)
        
        model4 <- train(classe~., training_proc, method="rf")
        pred_validation4 <- predict(model4, newdata = validation)
        pred_submission4 <- predict(model4, newdata = testing)
        
        model5 <- train(classe~., training_proc, method="rf")
        pred_validation5 <- predict(model5, newdata = validation)
        pred_submission5 <- predict(model5, newdata = testing)
        
        model6 <- train(classe~., training_proc, method="rf")
        pred_validation6 <- predict(model6, newdata = validation)
        pred_submission6 <- predict(model6, newdata = testing)
}

##################################################################
# Multiclass problems not supported yet in caretEnsemble
#

ensembleMulticlass <- function(){
        my_control <- trainControl(
                method="boot",
                number=25,
                savePredictions="final",
                classProbs=TRUE,
                index=createResample(training$classe, 25),
                summaryFunction=multiClassSummary
        )
        
        #Make a list of all the models
        model_list <- caretList(
                classe~., data=training,
                trControl=my_control,
                metric="ROC",
                #methodList=c("rpart", "rpart"),
                tuneList=list(
                        rf1=caretModelSpec(method="rpart", na.action = na.exclude),
                        rf2=caretModelSpec(method="rpart", na.action = na.exclude, preProcess="pca")
                )
        )
        
        p <- as.data.frame(predict(model_list, newdata=head(submission_data)))
        print(p)
        xyplot(resamples(model_list))
        modelCor(resamples(model_list))
        
        greedy_ensemble <- caretEnsemble(
                model_list, 
                metric="ROC",
                trControl=trainControl(
                        number=2,
                        summaryFunction=multiClassSummary,
                        classProbs=TRUE
                ))
        summary(greedy_ensemble)  
}

save.image("project.RData")