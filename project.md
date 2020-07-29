```R
library(caret)
```

    Warning message:
    "package 'caret' was built under R version 3.6.3"Loading required package: lattice
    Loading required package: ggplot2
    Warning message:
    "package 'ggplot2' was built under R version 3.6.3"


```R
library(rpart)
```


```R
library(rpart.plot)
```


    Error in library(rpart.plot): there is no package called 'rpart.plot'
    Traceback:
    

    1. library(rpart.plot)



```R
install.packages("rpart.plot", repos='http://cran.us.r-project.org')
```

    package 'rpart.plot' successfully unpacked and MD5 sums checked
    
    The downloaded binary packages are in
    	C:\Users\Acer\AppData\Local\Temp\RtmpgFPMMX\downloaded_packages
    


```R
library(rpart.plot)
```

    Warning message:
    "package 'rpart.plot' was built under R version 3.6.3"


```R
library(rpart.plot)
```

    Warning message:
    "package 'randomForest' was built under R version 3.6.3"randomForest 4.6-14
    Type rfNews() to see new features/changes/bug fixes.
    
    Attaching package: 'randomForest'
    
    The following object is masked from 'package:ggplot2':
    
        margin
    
    


```R
library(corrplot)
```


    Error in library(corrplot): there is no package called 'corrplot'
    Traceback:
    

    1. library(corrplot)



```R
install.packages("corrplot", repos='http://cran.us.r-project.org')
```

    package 'corrplot' successfully unpacked and MD5 sums checked
    
    The downloaded binary packages are in
    	C:\Users\Acer\AppData\Local\Temp\RtmpgFPMMX\downloaded_packages
    


```R
library(corrplot)
```

    Warning message:
    "package 'corrplot' was built under R version 3.6.3"corrplot 0.84 loaded
    


```R
trainingURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
trainingFile <- "./data/pml-training.csv"
testingFile  <- "./data/pml-testing.csv"
if (!file.exists("./data")) {
  dir.create("./data")
}
if (!file.exists(trainingFile)) {
  download.file(trainingURL, destfile=trainingFile, method="curl")
}
if (!file.exists(testingFile)) {
  download.file(testingURL, destfile=testingFile, method="curl")
}
```


```R
trainRaw <- read.csv("./data/pml-training.csv")
testRaw <- read.csv("./data/pml-testing.csv")
dim(trainRaw)
```


<ol class=list-inline>
	<li>19622</li>
	<li>160</li>
</ol>




```R
dim(testRaw)
```


<ol class=list-inline>
	<li>20</li>
	<li>160</li>
</ol>




```R
sum(complete.cases(trainRaw))
```


406



```R
trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0] 
```


```R
testRaw <- testRaw[, colSums(is.na(testRaw)) == 0] 
```


```R
classe <- trainRaw$classe
trainRemove <- grepl("^X|timestamp|window", names(trainRaw))
trainRaw <- trainRaw[, !trainRemove]
trainCleaned <- trainRaw[, sapply(trainRaw, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testRaw))
testRaw <- testRaw[, !testRemove]
testCleaned <- testRaw[, sapply(testRaw, is.numeric)]
```


```R
set.seed(2108) # For reproducibile purpose
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]
```


```R
controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
modelRf
```


    Error: package e1071 is required
    Traceback:
    

    1. train(classe ~ ., data = trainData, method = "rf", trControl = controlRf, 
     .     ntree = 250)

    2. train.formula(classe ~ ., data = trainData, method = "rf", trControl = controlRf, 
     .     ntree = 250)

    3. train(x, y, weights = w, ...)

    4. train.default(x, y, weights = w, ...)

    5. evalSummaryFunction(y, wts = weights, ctrl = trControl, lev = classLevels, 
     .     metric = metric, method = method)

    6. ctrl$summaryFunction(testOutput, lev, method)

    7. postResample(data[, "pred"], data[, "obs"])

    8. requireNamespaceQuietStop("e1071")

    9. stop(paste("package", package, "is required"), call. = FALSE)



```R
install.packages("e1071", repos='http://cran.us.r-project.org')
```

    package 'e1071' successfully unpacked and MD5 sums checked
    
    The downloaded binary packages are in
    	C:\Users\Acer\AppData\Local\Temp\RtmpgFPMMX\downloaded_packages
    


```R
controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
modelRf
```


    Random Forest 
    
    13737 samples
       52 predictor
        5 classes: 'A', 'B', 'C', 'D', 'E' 
    
    No pre-processing
    Resampling: Cross-Validated (5 fold) 
    Summary of sample sizes: 10989, 10989, 10990, 10990, 10990 
    Resampling results across tuning parameters:
    
      mtry  Accuracy   Kappa    
       2    0.9892259  0.9863692
      27    0.9913372  0.9890413
      52    0.9826747  0.9780829
    
    Accuracy was used to select the optimal model using the largest value.
    The final value used for the model was mtry = 27.



```R
predictRf <- predict(modelRf, testData)
confusionMatrix(testData$classe, predictRf)
```


    Confusion Matrix and Statistics
    
              Reference
    Prediction    A    B    C    D    E
             A 1671    1    1    0    1
             B   13 1124    2    0    0
             C    0    3 1022    1    0
             D    0    0    7  957    0
             E    0    0    4    3 1075
    
    Overall Statistics
                                              
                   Accuracy : 0.9939          
                     95% CI : (0.9915, 0.9957)
        No Information Rate : 0.2862          
        P-Value [Acc > NIR] : < 2.2e-16       
                                              
                      Kappa : 0.9923          
                                              
     Mcnemar's Test P-Value : NA              
    
    Statistics by Class:
    
                         Class: A Class: B Class: C Class: D Class: E
    Sensitivity            0.9923   0.9965   0.9865   0.9958   0.9991
    Specificity            0.9993   0.9968   0.9992   0.9986   0.9985
    Pos Pred Value         0.9982   0.9868   0.9961   0.9927   0.9935
    Neg Pred Value         0.9969   0.9992   0.9971   0.9992   0.9998
    Prevalence             0.2862   0.1917   0.1760   0.1633   0.1828
    Detection Rate         0.2839   0.1910   0.1737   0.1626   0.1827
    Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
    Balanced Accuracy      0.9958   0.9967   0.9928   0.9972   0.9988



```R
accuracy <- postResample(predictRf, testData$classe)
accuracy
```


<dl class=dl-horizontal>
	<dt>Accuracy</dt>
		<dd>0.993882752761257</dd>
	<dt>Kappa</dt>
		<dd>0.992261060800132</dd>
</dl>




```R
oose <- 1 - as.numeric(confusionMatrix(testData$classe, predictRf)$overall[1])
oose
```


0.00611724723874252



```R
result <- predict(modelRf, testCleaned[, -length(names(testCleaned))])
result
```


<ol class=list-inline>
	<li>B</li>
	<li>A</li>
	<li>B</li>
	<li>A</li>
	<li>A</li>
	<li>E</li>
	<li>D</li>
	<li>B</li>
	<li>A</li>
	<li>A</li>
	<li>B</li>
	<li>C</li>
	<li>B</li>
	<li>A</li>
	<li>E</li>
	<li>E</li>
	<li>A</li>
	<li>B</li>
	<li>B</li>
	<li>B</li>
</ol>

<details>
	<summary style=display:list-item;cursor:pointer>
		<strong>Levels</strong>:
	</summary>
	<ol class=list-inline>
		<li>'A'</li>
		<li>'B'</li>
		<li>'C'</li>
		<li>'D'</li>
		<li>'E'</li>
	</ol>
</details>



```R
corrPlot <- cor(trainData[, -length(names(trainData))])
corrplot(corrPlot, method="color")
```


![png](output_24_0.png)



```R
treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel)
```


![png](output_25_0.png)



```R

```
