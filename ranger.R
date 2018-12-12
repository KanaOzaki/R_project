library(MASS)
library(ranger)
library(dplyr)
library(caret)
library(randomForest)
data(Boston)

### インターン用　予習 ###

### Bostonデータにて、回帰問題をランダムフォレストを使って解く###
# 木の数のみ、もっともMSEが収束したところで判断し、
# 後はデフォルトパラメータのまま
# Cross Validationで評価
# caret が便利そうなので使って見る
# ntrees = 1000の時の実行時間約20秒

## 普通のrandomForest Ver. ---
rf <- randomForest(medv ~., data = Boston, ntree = 1000)
plot(rf)
#木の数は 400 でよさそう..

rf_grid <- expand.grid(mtry = 4)

modelRanger <- train(
  medv ~ ., 
  data = Boston,
  method = "rf", 
  ntree = 400,
  trControl = trainControl(method = "cv"),
  tuneGrid = rf_grid
)



## rangerでやった Ver.---
# データが多そうなので、できればrangerを使いたい

# 普通にやると...
rf <- ranger(medv ~., data = Boston, num.trees = 1000, mtry = 4)
rf$num.independent.variables

# caretを使って、パラメータはregressionのdefault値で固定して、cross validation
myControl <- trainControl(
    method = "cv", number = 5, 
    verboseIter = TRUE
)

# regressionのハイパーパラメータについて
# mtry : default はclassification, regresstion共通して√n だが、回帰にはn/3が良いそうなので、それで固定
# splitrule : regressionのdefault は variance. 他に extratrees か maxstat が選べる.
# min.node.size : regressionのdefault は 5.
# 実行時間は約4秒

rf_grid <- expand.grid(mtry = 4,
                       splitrule = "variance",
                       min.node.size = 5)
rf_grid

modelRanger <- train(
  medv ~ ., 
  data = Boston,
  method = "ranger", 
  num.trees = 1000,
  #tuneLength = 4,
  #preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv"),
  tuneGrid = rf_grid
)

