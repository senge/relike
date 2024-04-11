

require(caret)

# Generalized Linear Model
relike.train.fun.glm <- function(x, y) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="glm")
}

relike.predict.fun.glm <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}

# Gradient Boosting Model
relike.train.fun.gbm <- function(x, y) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="gbm")
}

relike.predict.fun.gbm <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}

# Naive Bayes
relike.train.fun.nb <- function(x, y) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="nb", tuneGrid=expand.grid(fL = 1, usekernel = FALSE, adjust = 0))
}

relike.predict.fun.nb <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}

# knn
relike.train.fun.knn <- function(x, y, k = 5) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="knn", tuneGrid=expand.grid(k = k))
}

relike.predict.fun.knn <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}

# J48
relike.train.fun.j48 <- function(x, y) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="J48")
}

relike.predict.fun.j48 <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}

# PART
relike.train.fun.part <- function(x, y) {
  cntrl = trainControl(method = "none", returnData = F)
  train(x, y, trControl=cntrl, method="PART")
}

relike.predict.fun.part <- function(m, newx) {
  as.matrix(predict(m, newdata = newx, type="prob"))
}