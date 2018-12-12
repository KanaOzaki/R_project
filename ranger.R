library(MASS)
library(ranger)
library(dplyr)
library(caret)
library(randomForest)
data(Boston)

### Bostonデータにて、回帰問題をランダムフォレストを使って解く###
#木の数のみ、もっともMSEが収束したところで判断し、
#後はデフォルトパラメータのまま
#Cross Validationで評価

# 普通のrandomForest Ver. ---
rf <- randomForest(medv ~., data = Boston, ntree = 1000)

# rangerでやって見たVer.



rf <- ranger(medv ~., data = Boston, num.trees = 1000)
rf$prediction.error

myControl <- trainControl(
    method = "cv", number = 5, 
    verboseIter = TRUE
)

modelRanger <- train(
  medv ~ ., 
  data = Boston,
  method = "ranger", 
  #tuneLength = 4,
  #preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)

