

library(pROC)
library(caret)
library(kernlab)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
# GOAL
#> predictionX <- predict(modelFitted_your_selected, newdata=testing ...)
#> predictionX
## where "testing" is the 20 rows data set
## 1, 2, 3, ... ... 19, 20
## A, B, C, ... ... D, E

pml_training <- read.csv("pml-training.csv",na.strings=c("NA","","#DIV/0!"))
dim(pml_training)
sum(is.na(pml_training))

summary(pml_training$classe)

d <- pml_training
i1 <- sapply(d,function(x)is.factor(x))
i1

# remove columns with all NAs
aa <- pml_training[, colSums(is.na(pml_training)) != nrow(pml_training)]
dim(aa)
sum(is.na(aa))

#only keep columns with at least 50% non-blanks
bb <- pml_training[, colSums(is.na(pml_training)) < nrow(pml_training) * 0.5]
dim(bb)
sum(is.na(bb))

# remove 1st 6 column which I don't think will have any impact on the prediction
bb[1:6] <- list(NULL)
dim(bb)

intrain <- createDataPartition(y=bb$classe, p=0.6,list=FALSE)
training <- bb[intrain,]
testing <- bb[-intrain,]

dim(training)
dim(testing)

sum(is.na(training))
set.seed(seed)

#decision Tree

DTmodFit <- train(classe ~ ., method="rpart", data=training)
DTpredict<-predict(DTmodFit, newdata=testing)
confusionMatrix(DTpredict, testing$classe)
fancyRpartPlot(DTmodFit$finalModel)

# random forest 

require(randomForest)
RFmodFit<-train(classe ~ ., method="rf",trControl=trainControl(method="cv",number=5), data=training)
RFpredict<-predict(RFmodFit, newdata=testing)
confusionMatrix(RFpredict, testing$classe)
fancyRpartPlot(RFmodFit$finalModel)

# the last bit

pml_write_files = function(x){
  n = length(x)
  for(i in 1:20){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_testing <- read.csv("pml-testing.csv",na.strings=c("NA","","#DIV/0!"))
dim(pml_testing)
str(pml_testing)

cc <- pml_testing[, colSums(is.na(pml_testing)) != nrow(pml_testing)]
dim(cc)
sum(is.na(cc))

cc[1:6] <- list(NULL)
dim(cc)

RFpredict2<-predict(RFmodFit, newdata=cc)
RFpredict2

pml_write_files(RFpredict2)

