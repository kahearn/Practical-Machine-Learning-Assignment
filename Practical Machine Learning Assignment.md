---
title: "Practical Machine Learning Assignment"
author: "Ng Kah Earn"
output: 
  html_document: 
    keep_md: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(cache = TRUE,warning=FALSE)
```

# Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. Data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants are use for this project. The 6 participants were asked to perform barbell lifts correctly and incorrectly in 5 different ways classified as A, B, C, D and E and recorded as "classe" variable in the training data.

The goal of this project is to predict the manner in which the 6 participants did the exercise with the given measurement from accelerometers provided in the test data.

# Data Analysis

## Reading Data

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

The training data for this project is downloaded from: 
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

Training data is loaded into the computer replacing blanks and "DIV/0" with NA.
```{r}
TrainData <- read.csv("pml-training.csv",na.strings = c("NA", "","#DIV/0!"))
```
The test data for this project is downloaded from:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

Test data is loaded into the computer replacing blanks and "DIV/0" with NA.
```{r}
TestData <- read.csv("pml-testing.csv",na.strings = c("NA", "","#DIV/0!"))
```

## Data Processing

We start with looking into the properties of the dataset
```{r}
str(TrainData)
```
The first step is to remove variables that are not measurements from accelerometers.
```{r}
TrainData1 <- TrainData[,-c(1:6)]
TestData1 <- TestData[,-c(1:6)]
```

Second step is to remove variables with more than 50% NA.

```{r}
is_data <- apply(!is.na(TrainData1),2,sum)>10000
training <- TrainData1[,is_data]
testing <- TestData1[,is_data]
```

# Data Modelling

We are building our model with the given training data. The training data is first partition into training set and test set. 60% of the training data is set for training of the model and 40% is set to test the model. 

```{r}
library(caret)
set.seed(3337)
inTrain <- createDataPartition(y= training$classe, p=0.60, list = FALSE)
training1 <- training[inTrain,]
testing1 <- training[-inTrain,]
```

## Prediction Tree Model

We will built the first model using the prediction tree algorithm. After building the model (pred_rpart); It is cross validate with testing set
```{r}
library(rpart)
modfit_rpart <- train(classe~., data=training1, method="rpart")
pred_rpart <- predict(modfit_rpart,testing1)
confusionMatrix(pred_rpart, testing1$classe)
```

## Random Forest Model

Next we built model using the random forest algorithm. After building the model (pred_rf); It is cross validate with testing set
```{r}
library(randomForest)
modfit_rf <- train(classe~.,
                   data=training1,
                   method="rf",
                   trControl=trainControl(method="cv",number=2),
                   prox=TRUE,
                   verbose=TRUE,
                   allowParallel=TRUE)
pred_rf <- predict(modfit_rf,testing1)
confusionMatrix(pred_rf, testing1$classe)
```

## Model Comparison

Both the models are compare in terms of accuracy, time needed to process and memory required

1. The prediction tree model gives accuracy of 52.18% and error rate of 47.82%. While the random forest model gives accuracy of 99.66% and error rate of 0.34%. Random forest model gives much higher accuracy

2. The prediction tree model takes around 1 minute to run while the random forest model takes around 13 minutes to run. The prediction tree model is much faster 

3. The prediction tree model takes memory space of 11.2 Mb while the random forest model takes 1.1 Gb. The random forest model is taking a lot more memory resources. We will need a much more powerful computer to use this model.

We have selected the random forest model due to the much better accuracy.

## Prediction of Test data

Using the random forest model, we predict the activities of the 6 participants and classified as A, B, C, D and E.

```{r}
TestData1$problem_id <- as.factor(TestData1$problem_id)
predict(modfit_rf,TestData1)
```

From the quiz submission, we managed predict all 20 activities correctly.

# Conclusion

In this project we managed to predict all the 20 activities in the Test data using the random forest model. The random forest Model has accuracy of 99.66% and out of samples error of 0.34%.

The random forest algorithm is a better prediction algorithm as compare to the prediction tree algorithm but taking longer time to compute the results and uses a lot more memory space.

