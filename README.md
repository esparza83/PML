---
#title: "Prediction Assignment - Course Project"
output: html_document
course: Practical Machine Learning
---

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

#Data load:
```{r}
#Packages require
require(doMC)   
registerDoMC(cores = 8)
require(data.table)   
require(caret)
require(randomForest)
#Load data and remove features with null values
training = read.csv(file = 'pml-training.csv',na.strings=c("NA","#DIV/0!", ""))
testing = read.csv(file= 'pml-testing.csv',na.strings=c("NA","#DIV/0!", ""))
training=training[,colSums(is.na(training)) == 0]
testing = testing[,colSums(is.na(testing)) == 0]
#Remove feactures
remove = c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")
training = training[,!(names(training) %in% remove)]
testing = testing[,!(names(testing) %in% remove)]
```

#Subset of data:
```{r}
#Create train and test subset of data. 60% 
trainIndex <- createDataPartition(training$classe,p=.6,list=FALSE)
trainData <- training[trainIndex,]
testData  <- training[-trainIndex,]
```

#Subset of data and cross validation:
```{r}
# Using the rfe funtion from the caract package, with 10 cv we obtained the optimum number of features: 52 variables are important
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
model.rfe <- rfe(trainData[,-53], trainData[,53], sizes=c(1:8), rfeControl=control)
print(model.rfe)
#With 52 variables we obtained the higer accuracy
```


#Model:
```{r}
#We are going to use Random Forest as our predictive model
Ctrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE)
newGrid = expand.grid(mtry = c(2,4,8,15))
model = train(classe ~ ., data = trainData, trControl = Ctrl, method = "rf", tuneGrid = newGrid)
#After doing cross-validation and repeat 3 times. The optimal value of the mtry is 8.
 print(model$results)
 #The optimal value of mtry is 8.
 print(model$bestTune)
 # OOB of  0.65%
 print(model$finalModel)
```
#Validation:
```{r}
#Using the model we are going to validate against the test data 
model.prediction = predict(model, testData)
#Confusion Matrix  
model.CM = confusionMatrix(model.prediction ,testData$classe)
#With an avg accuracy of :
print(model.CM$overall)
```
#Conclusion:
With 56 features and an Avg accuracy of 99.32% we can conclude that the model could be use to clasify future data
      
