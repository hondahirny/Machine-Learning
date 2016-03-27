
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","train.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","test.csv")

setwd("C:/Users/Marcin/Documents/Machine Learning/Class Project")

library(caret)
finalquiz <- read.csv("test.csv")
train_orig <- read.csv("train.csv")

nalist <- !is.na(train_orig[1,])
trainnona <- train_orig[,nalist]
final <- finalquiz[,nalist]

inTrain <- createDataPartition(y = trainnona$classe, p = 0.6, list = FALSE)
train <- trainnona[inTrain,]
test <- trainnona[-inTrain,]

nzero <- nearZeroVar(train,freqCut = 5, uniqueCut = 5 , saveMetrics = FALSE)
trainclean <- train[,-nzero]
testclean <- test[,-nzero]
final <- final[,-nzero]


#####
  library(rpart)
  set.seed(123456)
  trainmodel<-rpart(classe~.,data=trainclean[,7:59])
  predtest<-predict(trainmodel,newdata = testclean,type = "class")
  confusionMatrix(predtest,testclean$classe)
####
  tc <- trainControl(method = "cv")
  set.seed(123456)
  trainmodel2 <- train(classe~.,data = trainclean[,7:59], method = "lda",trControl = tc)
  predtest2 <- predict(trainmodel2,newdata = trainclean)
  confusionMatrix(predtest2,trainclean$classe)
###
  tc2 <- trainControl(method = "cv",number = 2, verboseIter = TRUE)
  tc2 <- trainControl(method = "none", verboseIter = TRUE)
  
  set.seed(123456)
  system.time(trainmodel3 <- train(trainclean[,7:58],trainclean[,59],method = "parRF", tuneLength = 1, trControl = tc2))
  trainmodel3 <- train(classe~.,data = trainclean[,7:59], method = "parRF",trControl = tc2)
  predtrain3 <- predict(trainmodel3,newdata = trainclean)
  confusionMatrix(predtrain3,trainclean$classe)
  predtest3 <- predict(trainmodel3,newdata = testclean)
  confusionMatrix(predtest3,testclean$classe)
  

  
###  
predict(trainmodel3,newdata = final)

qplot(trainclean[,83],color=trainclean$classe,geom="density")

cm <- abs(cov(trainclean[,7:87]))
diag(cm) <- 0
which(cm > 0.8, arr.ind = T )
prComp <- prcomp(trainclean[,7:87])
