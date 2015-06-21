# Practical Machine Learning Course Project - Prediction On Wearable Tech For Barbell lifts Using R
Kevin  
June 21, 2015  

## (1) Introduction

In this project, we will be using data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants and to predict the manner in which they did the exercise based on any of the other variables. The manner of exercise, which is the what we want to predict, is  denote as "classe" variable in the training set. 

Data can be downloaded from the following links: [Training Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv), [Test Data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv). More information is available from the website here: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset). 

We will be using Random Forest as the predictive model as it is one of the top performing algorithms used in prediction contest as recommended by the lecturers :)

## (2) Preprocessing of Data

#### Data Loading

First, we will download the data from the sources and load into R using read.csv function.


```r
# Loading of Data
# download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="pml-training.csv", method="curl")
# download.file("http://groupware.les.inf.puc-rio.br/har", destfile="pml-testing.csv", method="curl")
traindata <- read.csv("pml-training.csv", na.strings=c("","NA","DIV/0"))
testdata <- read.csv("pml-testing.csv", na.strings=c("","NA","DIV/0"))
```

#### Data Cleaing

Data is further processed removing all columns that contains NA elements.


```r
# Remove Unneccessary columns containing NA values
train_data <- traindata[names(traindata[colSums(is.na(traindata))==0])]
test_data <- testdata[names(testdata[colSums(is.na(testdata))==0])]
dim(train_data)
```

```
## [1] 19622    60
```

```r
str(train_data)
```

```
## 'data.frame':	19622 obs. of  60 variables:
##  $ X                   : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ user_name           : Factor w/ 6 levels "adelmo","carlitos",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ raw_timestamp_part_1: int  1323084231 1323084231 1323084231 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 1323084232 ...
##  $ raw_timestamp_part_2: int  788290 808298 820366 120339 196328 304277 368296 440390 484323 484434 ...
##  $ cvtd_timestamp      : Factor w/ 20 levels "02/12/2011 13:32",..: 9 9 9 9 9 9 9 9 9 9 ...
##  $ new_window          : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ num_window          : int  11 11 11 12 12 12 12 12 12 12 ...
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

The first 5 columns, "X", "Username", "raw_timestamp_part_1", "raw_timestamp_part_2" and "cvtd_timestamp" are removed as they play no part for the prediction.


```r
# Remove Unneccessary columns
train_data <- train_data[,-c(1:5)]
test_data <- test_data[,-c(1:5),]
```

Hooray! We have successfully reduce the number of variables from 160 to 55.

## (3) Data Prediction Modeling - Random Forest Algorithm

The training data set is further split into training data set (70%) and validation set (30%) using the createDataPartition function in caret package.


```r
# Loading of caret package
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.2.1
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
set.seed(1233)
inTrain <- createDataPartition(train_data$classe, p=0.7, list=FALSE)
training <- train_data[inTrain,]
validating <- train_data[-inTrain,]
```

Using the training set, we will develop the random forest model with the randomForest function in the randomForest package.


```r
# Loading of randomForest Package
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.2.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
# Training of random forest model
# Unable to use Proximity parameter due to 32-bit CPU thus getting insufficient memory error
modfit <- randomForest(classe~.,data=training)
modfit
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = training) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.28%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3906    0    0    0    0 0.000000000
## B    4 2650    4    0    0 0.003009782
## C    0    9 2386    1    0 0.004173623
## D    0    0   14 2237    1 0.006660746
## E    0    0    0    5 2520 0.001980198
```

We will then cross-validate the model by using the validation data set to test the accuracy of the prediction.


```r
pred <- predict(modfit, validating)
confusionMatrix(pred, validating$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    0    0    0    0
##          B    0 1138    7    0    0
##          C    0    1 1019    4    0
##          D    0    0    0  960    1
##          E    0    0    0    0 1081
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9978          
##                  95% CI : (0.9962, 0.9988)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9972          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   0.9991   0.9932   0.9959   0.9991
## Specificity            1.0000   0.9985   0.9990   0.9998   1.0000
## Pos Pred Value         1.0000   0.9939   0.9951   0.9990   1.0000
## Neg Pred Value         1.0000   0.9998   0.9986   0.9992   0.9998
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2845   0.1934   0.1732   0.1631   0.1837
## Detection Prevalence   0.2845   0.1946   0.1740   0.1633   0.1837
## Balanced Accuracy      1.0000   0.9988   0.9961   0.9978   0.9995
```

RandomForest model fits perfectly on the dataset with a 99.8% accuracy!

## (4) Conclusion

Using Random Forest, we are able to create a model that through cross-validation, yields a 99.8% accuracy and 0.2% in-sample error. This suggest that the model fits perfectly to the data set collected. However, although the model has such a high accuracy, this may be due to the nature of data that results an over-fitted model. Furthermore, we are predicting 5 types of outcomes with detection rate of an average of 20% each. We will definately expect an increase in out of sample error for other test sets/cases.

## (5) Prediction Results for the Prediction Assignment Submission

For prediction to work, both the training data set and the testing data set must have variables with the same data type. "new_window" variable in both data sets have different levels and thus has to change the levels for the variable in the test data sets.



```r
# Adding levels to test data set to prevent error during prediction
levels(train_data$new_window)
```

```
## [1] "no"  "yes"
```

```r
levels(test_data$new_window)
```

```
## [1] "no"
```

```r
levels(test_data$new_window) <- c("no","yes")
```

Predicting the test data sets with the Random Forest Model.


```r
predtest <- predict(modfit, test_data)
```

Generate the result files for the 20 test cases.


```r
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

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
