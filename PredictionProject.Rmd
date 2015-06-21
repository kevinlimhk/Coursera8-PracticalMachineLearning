---
title: "Practical Machine Learning Course Project - Prediction On Wearable Tech For Barbell lifts Using R"
author: "Kevin"
date: "June 21, 2015"
output: html_document
---

## (1) Introduction

In this project, we will be using data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants and to predict the manner in which they did the exercise based on any of the other variables. The manner of exercise, which is the what we want to predict, is  denote as "classe" variable in the training set. 

Data can be downloaded from the following links: [Training Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv), [Test Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv). More information is available from the website here: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset). 

We will be using Random Forest as the predictive model as it is one of the top performing algorithms used in prediction contest as recommended by the lecturers :)

## (2) Preprocessing of Data

#### Data Loading

First, we will download the data from the sources and load into R using read.csv function.

```{r, echo=TRUE}
# Loading of Data
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv", method="curl")
# download.file("http://groupware.les.inf.puc-rio.br/har", destfile="pml-testing.csv", method="curl")
traindata <- read.csv("pml-training.csv", na.strings=c("","NA","DIV/0"))
testdata <- read.csv("pml-testing.csv", na.strings=c("","NA","DIV/0"))
```

#### Data Cleaing

Data is further processed removing all columns that contains NA elements.

```{r, echo=TRUE}
# Remove Unneccessary columns containing NA values
train_data <- traindata[names(traindata[colSums(is.na(traindata))==0])]
test_data <- testdata[names(testdata[colSums(is.na(testdata))==0])]
dim(train_data)
str(train_data)
```

The first 5 columns, "X", "Username", "raw_timestamp_part_1", "raw_timestamp_part_2" and "cvtd_timestamp" are removed as they play no part for the prediction.

```{r, echo=TRUE}
# Remove Unneccessary columns
train_data <- train_data[,-c(1:5)]
test_data <- test_data[,-c(1:5),]
```

Hooray! We have successfully reduce the number of variables from 160 to 55.

## (3) Data Prediction Modeling - Random Forest Algorithm

The training data set is further split into training data set (70%) and validation set (30%) using the createDataPartition function in caret package.

```{r, echo=TRUE}
# Loading of caret package
library(caret)
set.seed(1233)
inTrain <- createDataPartition(train_data$classe, p=0.7, list=FALSE)
training <- train_data[inTrain,]
validating <- train_data[-inTrain,]
```

Using the training set, we will develop the random forest model with the randomForest function in the randomForest package.

```{r, echo=TRUE}
# Loading of randomForest Package
library(randomForest)
# Training of random forest model
# Unable to use Proximity parameter due to 32-bit CPU thus getting insufficient memory error
modfit <- randomForest(classe~.,data=training)
modfit
```

We will then cross-validate the model by using the validation data set to test the accuracy of the prediction.

```{r, echo=TRUE}
pred <- predict(modfit, validating)
confusionMatrix(pred, validating$classe)
```

RandomForest model fits perfectly on the dataset with a 99.8% accuracy!

## (4) Conclusion

Using Random Forest, we are able to create a model that through cross-validation, yields a 99.8% accuracy and 0.2% in-sample error. This suggest that the model fits perfectly to the data set collected. However, although the model has such a high accuracy, this may be due to the nature of data that results an over-fitted model. Furthermore, we are predicting 5 types of outcomes with detection rate of an average of 20% each. We will definately expect an increase in out of sample error for other test sets/cases.

## (5) Prediction Results for the Prediction Assignment Submission

For prediction to work, both the training data set and the testing data set must have variables with the same data type. "new_window" variable in both data sets have different levels and thus has to change the levels for the variable in the test data sets.


```{r, echo=TRUE}
# Adding levels to test data set to prevent error during prediction
levels(train_data$new_window)
levels(test_data$new_window)
levels(test_data$new_window) <- c("no","yes")
```

Predicting the test data sets with the Random Forest Model.

```{r, echo=TRUE}
predtest <- predict(modfit, test_data)
```

Generate the result files for the 20 test cases.

```{r, echo=TRUE}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predtest)
predtest
```