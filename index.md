# Quantification of excercise quality
Bruno Blazinc  
28 Apr 2016  


```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
## 
## Loading required package: doParallel
## Loading required package: foreach
## Loading required package: iterators
## Loading required package: parallel
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: doMC
```


## Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity but the users usually use the fitness trackers to quantify how much of a certain exercise they do, but rarely how well they performed the exercise. In this document we will try to answer that question by training_data a model that can quantify the quality of a certain exercise or activity. For this purpose we will use the data obtained from [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har). The data set consists of data for 6 subjects, collected from 4 accelerometers on the belt, forearm, arm, and dumbbell. The subjects were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Class A represents the correct way to perform the exercise and the other four common mistakes.


## Reading the data and preprocessing
First we download the training_data and test data from following links and read it into two dataframes.

* training_data data[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training_data.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training_data.csv)
* Test data[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)



The data has features without any data except where the feature ```new_window=="yes"``` so first we will filter only observations where the value is ```new_window=="no"```. The ```new_window``` has 19210 ```no``` and 406 ```yes``` observations, so it's safe to assume we can remove observations where the value is ```yes```. Next we remove the features where all the values are NA, and remove the features we won't need(eg. ordinal number, date, etc.). After cleaning up the data we will check if there remain any variables with near zero variance with the method ```nearZeroVar()```.


```
##    no   yes 
## 19216   406
```

```
## integer(0)
```

## Model building
First the ```training_data``` is split in 3 parts, 70% for training, 15% for validation and 15% for testing. We will train 3 types of models each with and without preprocessing, so we can see the benefit of the preprocessing. Next we will train a classification tree(```rpart```), a general boosted model(```gbm```), and random forest(```rf```), and  for preprocessing we will ```center``` and ```scale``` the data. For the train control we will use the 10-fold cross-validation.


```
## Loading required package: rpart
## Loading required package: gbm
## Loading required package: survival
## 
## Attaching package: 'survival'
## 
## The following object is masked from 'package:caret':
## 
##     cluster
## 
## Loading required package: splines
## Loaded gbm 2.1.1
## Loading required package: plyr
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
## 
## Attaching package: 'plyr'
## 
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## Loading required package: randomForest
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:ggplot2':
## 
##     margin
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
##   methods    preprocess  accuracy
## 1   rpart               0.5168228
## 2     gbm               0.9857787
## 3      rf               0.9979188
## 4   rpart center, scale 0.5168228
## 5     gbm center, scale 0.9885536
## 6      rf center, scale 0.9975720
```

Now we can look at the accuracies of each model. Take note that all the model pairs(with and without preprocessing) have the same accuracy. That is because we used the type of models that handle non-normal distributions pretty well. The worst accuracy is from the ```rpart``` model 0.516, followed by the ```gbm``` model with an accuracy of 0.988, and finally the ```rf``` model with an accuracy of 0.998 . The accuracies are calculated on the validation data set. We will proceed by aggregating the results of the models and training an ensemble model with them. For the ensemble model we chose a general boosted model.


```
## Source: local data frame [2,883 x 61]
## 
##    user_name num_window roll_belt pitch_belt yaw_belt total_accel_belt
##       (fctr)      (int)     (dbl)      (dbl)    (dbl)            (int)
## 1   carlitos         11      1.41       8.07    -94.4                3
## 2   carlitos         12      1.45       8.06    -94.4                3
## 3   carlitos         12      1.45       8.17    -94.4                3
## 4   carlitos         13      1.44       8.18    -94.3                3
## 5   carlitos         13      1.34       8.05    -94.3                3
## 6   carlitos         13      1.30       7.85    -94.2                3
## 7   carlitos         14      1.26       7.54    -94.1                3
## 8   carlitos         14      1.26       7.47    -94.2                3
## 9   carlitos         14      1.27       7.46    -94.1                3
## 10  carlitos         14      1.27       7.35    -94.1                3
## ..       ...        ...       ...        ...      ...              ...
## Variables not shown: gyros_belt_x (dbl), gyros_belt_y (dbl), gyros_belt_z
##   (dbl), accel_belt_x (int), accel_belt_y (int), accel_belt_z (int),
##   magnet_belt_x (int), magnet_belt_y (int), magnet_belt_z (int), roll_arm
##   (dbl), pitch_arm (dbl), yaw_arm (dbl), total_accel_arm (int),
##   gyros_arm_x (dbl), gyros_arm_y (dbl), gyros_arm_z (dbl), accel_arm_x
##   (int), accel_arm_y (int), accel_arm_z (int), magnet_arm_x (int),
##   magnet_arm_y (int), magnet_arm_z (int), roll_dumbbell (dbl),
##   pitch_dumbbell (dbl), yaw_dumbbell (dbl), total_accel_dumbbell (int),
##   gyros_dumbbell_x (dbl), gyros_dumbbell_y (dbl), gyros_dumbbell_z (dbl),
##   accel_dumbbell_x (int), accel_dumbbell_y (int), accel_dumbbell_z (int),
##   magnet_dumbbell_x (int), magnet_dumbbell_y (int), magnet_dumbbell_z
##   (dbl), roll_forearm (dbl), pitch_forearm (dbl), yaw_forearm (dbl),
##   total_accel_forearm (int), gyros_forearm_x (dbl), gyros_forearm_y (dbl),
##   gyros_forearm_z (dbl), accel_forearm_x (int), accel_forearm_y (int),
##   accel_forearm_z (int), magnet_forearm_x (int), magnet_forearm_y (dbl),
##   magnet_forearm_z (dbl), classe (fctr), model1 (fctr), model2 (fctr),
##   model3 (fctr), model4 (fctr), model5 (fctr), model6 (fctr)
```

```
## Source: local data frame [2,880 x 61]
## 
##    user_name num_window roll_belt pitch_belt yaw_belt total_accel_belt
##       (fctr)      (int)     (dbl)      (dbl)    (dbl)            (int)
## 1   carlitos         12      1.48       8.15    -94.4                3
## 2   carlitos         13      1.53       8.11    -94.4                3
## 3   carlitos         13      1.55       8.09    -94.4                3
## 4   carlitos         14      1.16       7.27    -94.1                3
## 5   carlitos         14      1.18       7.26    -94.1                3
## 6   carlitos         15      1.18       7.28    -94.1                3
## 7   carlitos         15      1.09       7.31    -94.1                3
## 8   carlitos         15      1.06       7.41    -94.1                3
## 9   carlitos         15      1.03       7.49    -94.0                3
## 10  carlitos         15      1.00       7.50    -94.1                3
## ..       ...        ...       ...        ...      ...              ...
## Variables not shown: gyros_belt_x (dbl), gyros_belt_y (dbl), gyros_belt_z
##   (dbl), accel_belt_x (int), accel_belt_y (int), accel_belt_z (int),
##   magnet_belt_x (int), magnet_belt_y (int), magnet_belt_z (int), roll_arm
##   (dbl), pitch_arm (dbl), yaw_arm (dbl), total_accel_arm (int),
##   gyros_arm_x (dbl), gyros_arm_y (dbl), gyros_arm_z (dbl), accel_arm_x
##   (int), accel_arm_y (int), accel_arm_z (int), magnet_arm_x (int),
##   magnet_arm_y (int), magnet_arm_z (int), roll_dumbbell (dbl),
##   pitch_dumbbell (dbl), yaw_dumbbell (dbl), total_accel_dumbbell (int),
##   gyros_dumbbell_x (dbl), gyros_dumbbell_y (dbl), gyros_dumbbell_z (dbl),
##   accel_dumbbell_x (int), accel_dumbbell_y (int), accel_dumbbell_z (int),
##   magnet_dumbbell_x (int), magnet_dumbbell_y (int), magnet_dumbbell_z
##   (dbl), roll_forearm (dbl), pitch_forearm (dbl), yaw_forearm (dbl),
##   total_accel_forearm (int), gyros_forearm_x (dbl), gyros_forearm_y (dbl),
##   gyros_forearm_z (dbl), accel_forearm_x (int), accel_forearm_y (int),
##   accel_forearm_z (int), magnet_forearm_x (int), magnet_forearm_y (dbl),
##   magnet_forearm_z (dbl), classe (fctr), model1 (fctr), model2 (fctr),
##   model3 (fctr), model4 (fctr), model5 (fctr), model6 (fctr)
```

```
## Source: local data frame [20 x 60]
## 
##    user_name num_window roll_belt pitch_belt yaw_belt total_accel_belt
##       (fctr)      (int)     (dbl)      (dbl)    (dbl)            (int)
## 1      pedro         74    123.00      27.00    -4.75               20
## 2     jeremy        431      1.02       4.87   -88.90                4
## 3     jeremy        439      0.87       1.82   -88.50                5
## 4     adelmo        194    125.00     -41.60   162.00               17
## 5     eurico        235      1.35       3.33   -88.60                3
## 6     jeremy        504     -5.92       1.59   -87.70                4
## 7     jeremy        485      1.20       4.44   -87.30                4
## 8     jeremy        440      0.43       4.15   -88.50                4
## 9   carlitos        323      0.93       6.72   -93.70                4
## 10   charles        664    114.00      22.40   -13.10               18
## 11  carlitos        859      0.92       5.94   -92.70                3
## 12    jeremy        461      1.01       4.96   -87.80                5
## 13    eurico        257      0.54       2.45   -88.60                3
## 14    jeremy        408      0.45       5.02   -87.90                5
## 15    jeremy        779      5.34      -3.09   -80.30                4
## 16    eurico        302      1.65       3.47   -87.00                2
## 17     pedro         48    129.00      27.80     1.84               21
## 18  carlitos        361      0.92       5.31   -93.10                3
## 19     pedro         72    123.00      26.70    -2.68               19
## 20    eurico        255      1.40       3.20   -88.70                3
## Variables not shown: gyros_belt_x (dbl), gyros_belt_y (dbl), gyros_belt_z
##   (dbl), accel_belt_x (int), accel_belt_y (int), accel_belt_z (int),
##   magnet_belt_x (int), magnet_belt_y (int), magnet_belt_z (int), roll_arm
##   (dbl), pitch_arm (dbl), yaw_arm (dbl), total_accel_arm (int),
##   gyros_arm_x (dbl), gyros_arm_y (dbl), gyros_arm_z (dbl), accel_arm_x
##   (int), accel_arm_y (int), accel_arm_z (int), magnet_arm_x (int),
##   magnet_arm_y (int), magnet_arm_z (int), roll_dumbbell (dbl),
##   pitch_dumbbell (dbl), yaw_dumbbell (dbl), total_accel_dumbbell (int),
##   gyros_dumbbell_x (dbl), gyros_dumbbell_y (dbl), gyros_dumbbell_z (dbl),
##   accel_dumbbell_x (int), accel_dumbbell_y (int), accel_dumbbell_z (int),
##   magnet_dumbbell_x (int), magnet_dumbbell_y (int), magnet_dumbbell_z
##   (int), roll_forearm (dbl), pitch_forearm (dbl), yaw_forearm (dbl),
##   total_accel_forearm (int), gyros_forearm_x (dbl), gyros_forearm_y (dbl),
##   gyros_forearm_z (dbl), accel_forearm_x (int), accel_forearm_y (int),
##   accel_forearm_z (int), magnet_forearm_x (int), magnet_forearm_y (int),
##   magnet_forearm_z (int), model1 (fctr), model2 (fctr), model3 (fctr),
##   model4 (fctr), model5 (fctr), model6 (fctr)
```

```
## Warning in gbm.fit(x = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, :
## variable 3: model1D has no variation.
```

```
## Warning in gbm.fit(x = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, :
## variable 15: model4D has no variation.
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

## Results and conclusion
After training the ensemble we calculate it's accuracy and RMSE on the test data set. The accuracy is 0.997 and RMSE is 0.045. That is quite good, though the standalone ```rf``` model has a marginally higher accuracy, but that is expected when using model ensembles.
The conclusion is that this model is quite good at predicting when the exercise was performed correctly or incorecctly.











