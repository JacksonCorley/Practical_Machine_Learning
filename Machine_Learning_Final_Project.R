


rm(list=ls()) 
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(knitr)
library(rattle)
library(randomForest)
library(corrplot)
library(gbm)
library(dplyr)
install.packages("e1071")



trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
validUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

train_data <- read.csv(url(trainUrl))
valid_data <- read.csv(url(validUrl))
dim(train_data)
dim(valid_data)

train_data <- train_data[, colSums(is.na(train_data)) == 0]
valid_data <- valid_data[, colSums(is.na(valid_data)) == 0]
dim(train_data)
dim(valid_data)

train_data <- train_data[, -c(1:7)]
valid_data <- valid_data[, -c(1:7)]
dim(train_data)
dim(valid_data)

set.seed(12345)
inTrain <- createDataPartition(train_data$classe, p = 0.7, list = FALSE)
train_data <- train_data[inTrain,]
test_Data <- train_data[-inTrain,]
dim(train_data)
dim(test_Data)

NZV <- nearZeroVar(train_data)
train_data <- train_data[, -NZV]
test_Data  <- test_Data[, -NZV]
dim(train_data)
dim(test_Data)

corr_matr <- cor(train_data[, -53])
corrplot(corr_matr, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = rgb(0, 0, 1))

str_corr = findCorrelation(corr_matr, cutoff=0.75)
names(train_data)[str_corr]

##classification trees
set.seed(12345)
class_tree_M1 <- rpart(classe ~ ., data=train_data, method="class")
fancyRpartPlot(class_tree_M1)

pred_tree_M1 <- predict(class_tree_M1, test_Data, type = "class")
cm_tree <- confusionMatrix(pred_tree_M1, test_Data$classe)
cm_tree

plot(cm_tree$table, col = cm_tree$byClass, 
     main = paste("Decision Tree: Accuracy =", round(cm_tree$overall['Accuracy'], 2)))

## random forests
set.seed(12345)
rand_forst_cont <- trainControl(method="cv", number=3, verboseIter=FALSE)
rand_forst_M1 <- train(classe ~ ., data=train_data, method="rf", trControl=rand_forst_cont)

rand_forst_pred <- predict(rand_forst_M1, newdata=test_Data)
cm_rand_forst <- confusionMatrix(rand_forst_pred, test_Data$classe)
cm_rand_forst

plot(cm_rand_forst$table, col = cm_rand_forst$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(cmrf$overall['Accuracy'], 2)))

## Generalized Boosting
set.seed(12345)
gen_boost_ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
gen_boost_M1  <- train(classe ~ ., data=train_data, method = "gbm", trControl = gen_boost_ctrl, verbose = FALSE)
gen_boost_pred <- predict(gen_boost_M1, newdata=test_Data)
cm_gen_boost <- confusionMatrix(gen_boost_pred, test_Data$classe)
cm_gen_boost

Results <- predict(rand_forst_M1, newdata=valid_data)
Results


