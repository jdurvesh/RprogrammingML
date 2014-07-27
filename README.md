RprogrammingML
==============

#Machine Learning Assignment in R prog. for Coursera
# Preprocessing of Data 
The data provided in the file has number of N/A values and variables that add little value to the classification program.
These variables are identified and filtered using the nearZeroVar command and then removed from the list. The model is then built on the 
> library(caret)> 
nzv<-nearZeroVar(pml.training)
> nzv
 [1]   6  12  13  14  15  16  17  20  23  26  51  52  53  54  55  56  57
[18]  58  59  69  70  71  72  73  74  75  78  79  81  82  87  88  89  90
[35]  91  92  95  98 101 125 126 127 128 129 130 131 133 134 136 137 139
[52] 142 143 144 145 146 147 148 149 150

# Build the training/validate/test datasets.
set.seed(44) 
crs$nobs <- nrow(crs$dataset) 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.8*crs$nobs) 
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) 

# The following variable selections have been noted.

crs$input <- c("num_window", "roll_belt", "pitch_belt", "yaw_belt",
     "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z",
     "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x",
     "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm",
     "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y",
     "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z",
     "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell",
     "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x",
     "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y",
     "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z",
     "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm",
     "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x",
     "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y",
     "magnet_forearm_z")

crs$numeric <- c("num_window", "roll_belt", "pitch_belt", "yaw_belt",
     "total_accel_belt", "gyros_belt_x", "gyros_belt_y", "gyros_belt_z",
     "accel_belt_x", "accel_belt_y", "accel_belt_z", "magnet_belt_x",
     "magnet_belt_y", "magnet_belt_z", "roll_arm", "pitch_arm",
     "yaw_arm", "total_accel_arm", "gyros_arm_x", "gyros_arm_y",
     "gyros_arm_z", "accel_arm_x", "accel_arm_y", "accel_arm_z",
     "magnet_arm_x", "magnet_arm_y", "magnet_arm_z", "roll_dumbbell",
     "pitch_dumbbell", "yaw_dumbbell", "total_accel_dumbbell", "gyros_dumbbell_x",
     "gyros_dumbbell_y", "gyros_dumbbell_z", "accel_dumbbell_x", "accel_dumbbell_y",
     "accel_dumbbell_z", "magnet_dumbbell_x", "magnet_dumbbell_y", "magnet_dumbbell_z",
     "roll_forearm", "pitch_forearm", "yaw_forearm", "total_accel_forearm",
     "gyros_forearm_x", "gyros_forearm_y", "gyros_forearm_z", "accel_forearm_x",
     "accel_forearm_y", "accel_forearm_z", "magnet_forearm_x", "magnet_forearm_y",
     "magnet_forearm_z")

crs$target  <- "classe"

# Build the Random Forest model.
set.seed(crv$seed)
crs$rf <- randomForest(classe ~ .,
      data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
      ntree=500,
      mtry=10,
      importance=TRUE,
      na.action=na.roughfix,
      replace=FALSE)

## Output of the Model

randomForest(formula = classe ~ .,
              data = crs$dataset[crs$sample, c(crs$input, crs$target)],
              ntree = 500, mtry = 10, importance = TRUE, replace = FALSE, na.action = na.roughfix)

               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 10

        OOB estimate of  error rate: 0.15%      # classification accuracy  error by random forest
Confusion matrix:
     A    B    C    D    E  class.error
A 4448    1    0    0    0 0.0002247696
B    4 3025    1    0    0 0.0016501650
C    0    5 2725    1    0 0.0021969974
D    0    0    9 2588    0 0.0034655372
E    0    0    0    3 2887 0.0010380623

Variable Importance
===================

                         A     B     C     D     E MeanDecreaseAccuracy
num_window           40.43 56.58 65.60 46.40 47.76                61.55
magnet_dumbbell_z    45.07 41.33 48.55 40.70 36.11                54.65
magnet_dumbbell_y    37.00 37.49 46.92 38.08 33.58                44.84
roll_belt            36.97 51.72 41.65 47.13 43.72                54.49
pitch_forearm        32.05 33.90 40.65 35.47 34.13                41.51
pitch_belt           28.01 47.06 39.07 32.69 31.03                47.11
yaw_belt             36.50 46.31 39.06 47.46 33.43                60.27
roll_forearm         29.56 25.59 29.37 22.99 24.02                28.48
accel_dumbbell_y     25.44 26.69 28.11 25.96 25.74                33.42
roll_dumbbell        21.80 26.20 27.24 27.63 25.44                29.34
magnet_dumbbell_x    21.71 24.99 26.59 24.07 22.21                26.09
gyros_dumbbell_y     19.11 19.73 26.06 18.43 17.18                23.86
magnet_belt_y        20.44 27.90 24.92 25.58 22.53                29.42
accel_dumbbell_z     18.50 25.55 24.17 25.30 25.70                31.09
gyros_arm_y          18.69 27.36 23.63 23.01 20.83                37.56
accel_forearm_x      17.68 25.33 23.59 28.47 24.03                26.62
magnet_forearm_z     21.68 26.82 23.43 22.99 23.15                30.56
gyros_belt_z         19.61 26.69 23.23 20.68 23.13                29.28
gyros_forearm_y      13.12 25.63 23.23 19.09 16.49                29.80
magnet_belt_x        15.39 26.16 22.97 20.57 24.70                32.80
yaw_arm              20.29 22.35 22.54 25.63 16.78                27.37
yaw_dumbbell         15.87 23.87 22.51 22.70 21.74                28.53
accel_forearm_y      16.17 20.26 22.42 16.44 19.68                26.24
roll_arm             19.40 26.25 22.21 25.84 17.89                29.22
gyros_arm_x          14.55 23.04 22.14 21.66 20.43                26.76
accel_forearm_z      16.28 24.25 21.79 20.33 22.26                28.61
accel_belt_z         22.03 24.60 21.77 21.47 18.50                27.88
gyros_dumbbell_x     16.50 23.00 21.55 18.76 18.27                35.49
magnet_belt_z        20.36 24.79 20.98 27.47 23.39                28.06
magnet_forearm_y     17.91 20.88 20.74 21.01 19.27                26.09
total_accel_dumbbell 18.11 23.25 20.07 21.97 24.38                26.17
accel_dumbbell_x     13.24 21.54 19.56 19.95 18.77                23.39
magnet_arm_z         16.02 23.57 19.29 18.94 18.77                25.24
yaw_forearm          14.68 20.82 19.16 18.77 19.08                25.94
pitch_arm            15.05 22.35 18.87 18.25 16.21                24.63
gyros_forearm_z      13.31 21.04 18.57 16.06 13.48                27.58
gyros_belt_x         16.53 14.53 17.86 12.18 16.93                25.73
accel_arm_z          10.57 15.61 17.78 17.37 15.09                17.54
magnet_arm_y         12.24 16.79 17.70 22.38 13.81                18.61
accel_arm_x          14.98 17.39 17.51 21.17 14.87                19.02
gyros_forearm_x       7.81 17.43 17.08 13.42 13.55                24.53
magnet_forearm_x     13.38 19.73 17.01 15.98 20.53                21.02
total_accel_arm       8.60 21.55 16.70 17.27 16.87                24.22
magnet_arm_x         13.88 13.79 16.25 15.62 12.87                15.42
total_accel_forearm  15.16 16.59 16.19 14.38 15.03                21.07
accel_belt_x         12.60 16.20 15.19 11.89 12.87                19.35
gyros_dumbbell_z     15.01 20.83 15.12 16.76 13.17                29.84
accel_arm_y          16.71 20.72 14.68 17.71 14.52                26.17
pitch_dumbbell       10.70 19.21 14.60 12.24 12.94                16.00
gyros_belt_y          9.96 12.30 13.72 13.07 16.55                16.31
gyros_arm_z           9.52 15.37 12.38 10.19  8.89                17.92
accel_belt_y         11.15 13.66 12.04 15.84 12.06                16.87
total_accel_belt     12.61 12.73 11.55 12.83 13.46                14.41
                     MeanDecreaseGini
num_window                     838.45
magnet_dumbbell_z              377.65
magnet_dumbbell_y              363.71
roll_belt                      665.58
pitch_forearm                  409.35
pitch_belt                     360.50
yaw_belt                       434.15
roll_forearm                   308.87
accel_dumbbell_y               189.46
roll_dumbbell                  196.56
magnet_dumbbell_x              220.42
gyros_dumbbell_y                94.82
magnet_belt_y                  183.15
accel_dumbbell_z               149.17
gyros_arm_y                     51.16
accel_forearm_x                149.80
magnet_forearm_z               124.71
gyros_belt_z                   115.44
gyros_forearm_y                 43.29
magnet_belt_x                  106.34
yaw_arm                         97.25
yaw_dumbbell                   111.43
accel_forearm_y                 51.51
roll_arm                       133.22
gyros_arm_x                     43.90
accel_forearm_z                101.94
accel_belt_z                   187.30
gyros_dumbbell_x                45.63
magnet_belt_z                  167.04
magnet_forearm_y                83.77
total_accel_dumbbell           135.08
accel_dumbbell_x               106.24
magnet_arm_z                    65.04
yaw_forearm                     69.56
pitch_arm                       66.94
gyros_forearm_z                 28.47
gyros_belt_x                    36.05
accel_arm_z                     47.67
magnet_arm_y                    89.75
accel_arm_x                    108.01
gyros_forearm_x                 24.18
magnet_forearm_x                83.63
total_accel_arm                 35.60
magnet_arm_x                    93.96
total_accel_forearm             36.19
accel_belt_x                    47.81
gyros_dumbbell_z                26.76
accel_arm_y                     53.97
pitch_dumbbell                  71.62
gyros_belt_y                    44.69
gyros_arm_z                     19.31
accel_belt_y                    59.00
total_accel_belt                90.58

##  Preicting Classes for the 20 variables for the Test Set
crs$pr <- predict(crs$rf, newdata=na.omit(crs$testset[,c(crs$input)]))
sdata <- subset(crs$testset[,], select=c("classe"))

write.csv(cbind(sdata, crs$pr), file="C:\Users\JavaleD\Desktop\train_score_idents10.csv", row.names=FALSE)

# Preidction output from the Word File 
rf
B
A
B
A
A
E
D
B
A
A
B
C
B
A
E
E
A
B
B
B


