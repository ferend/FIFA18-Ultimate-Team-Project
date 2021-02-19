

install.packages("ggplot2")
install.packages("tidyr")
install.packages("MASS")
install.packages("ISLR")
install.packages("forecast")

library(MASS)
library(ISLR)
library(forecast)
library(tidyr)
library(ggplot2)

setwd()
getwd()

options(scipen=999)

Players<-read.csv2("Final_Project_Data_Set_2021.csv",stringsAsFactors = F, sep=",",dec=".")
names(Players)


# a ) First drop all goalkeepers from the dataset. 
#If a player has a gk attribute, then it means he is a goalkeeper.

dataset <- Players[is.na(Players$gk), ]

# b ) Analyze your predictors/independent variables using techniques shown in the class.
  
      # load library

library(PerformanceAnalytics)
library(corrplot)

summary(dataset)
 
correlations <- cor(dataset[,c (3 , 12,17,20,21)])
corrplot(correlations, method="circle")

pairs (dataset[,c (3 , 12,17,20,21)])
chart.Correlation(dataset[,c (3 , 12,17,20,21)], histogram=TRUE, pch=19)

# c ) Take the first 1000 observation as the training set and use the rest as the test set.

dataset2 <- dataset[c(3:5,9:36)]
train <- dataset2[1:1000 , ]
test <- dataset2[1001:nrow(dataset2),]
names(dataset2)

# d ) Fit a multiple regression model to predict “eur_value”. Use only the training set to fit the regression model.

lm.fit <- lm(formula = eur_value ~.  , data = train  )
confint(lm.fit)
coef(lm.fit)
  
    #example plot
#plot <- ggplot(data = dataset2 , aes( vision ,eur_value  )) +
 # geom_point(alpha = 0.5)+
 # geom_smooth(method = "lm" , se = FALSE)
#plot

#train_coef<-coef(lm.fit)
#train_coef<-matrix(data=train_coef, nrow=nrow(test),ncol=nrow(test), byrow = TRUE)
#train_coef

# e) Analyze your estimated regression models. Comment on coefficients, adjusted R square and F statistic of the model. 

summary(lm.fit)

#8821000 / mean(dataset2$eur_value) # mistake ratio: residual standart error divided to mean of eur_value 


# f) Predict “eur_value” in the test set using the regression model obtained in (d).
#Calculate the mean square error of the test set (MSE).
  
testpredict<-predict(lm.fit, newdata=test)
summary(testpredict)

#MSE
RMSE<-sqrt((1/nrow(test))*sum((test$eur_value-testpredict)^2))
RMSE


#g) Fit a Ridge model and a Lasso model to predict “eur_value”. Use only the training set to
#fit these regression models. Determine the lambda parameter using cross-validation.

library(glmnet)

 #Ridge
set.seed(1)
train.mat <- model.matrix(eur_value ~ ., data = train)
test.mat <- model.matrix(eur_value ~ ., data = test)

grid <- 10 ^ seq(10, -2, length = 100)
fit.ridge <- glmnet(train.mat, train$eur_value, alpha = 0, lambda = grid)
cv.ridge <- cv.glmnet(train.mat, train$eur_value, alpha = 0, lambda = grid)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
plot(cv.ridge)
log(bestlam.ridge)
    
#Lasso
set.seed(1)
grid <- 10 ^ seq(10, -2, length = 100)
fit.lasso <- glmnet(train.mat, train$eur_value, alpha = 1, lambda = grid)
cv.lasso <- cv.glmnet(train.mat, train$eur_value, alpha = 1, lambda = grid)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
plot(cv.lasso)
log(bestlam.lasso)


#h) Analyze your Lasso Model. Compare your Lasso Model with the multiple regression model estimated in (d).

predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
plot(cv.lasso$glmnet.fit, "lambda", label=TRUE)

#i) Predict “eur_value” in the test set using the Ridge model and the Lasso model obtained in (g). 
#Calculate MSEs of these models only using the test set.

pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
RMSE2 <- sqrt(mean((pred.ridge - test$eur_value)^2))
RMSE2

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
RMSE3 <- sqrt(mean((pred.lasso - test$eur_value)^2))
RMSE3

#j) Fit a regression tree to predict “eur_value”. Use only the training set to fit the regression 
# model. Determine the number of terminal nodes using cross-validation.


library(tree)
set.seed(1)
tree.players <- tree(eur_value~., data = train)
summary(tree.players)
plot(tree.players)
text(tree.players)

cv.players <- cv.tree(tree.players)
plot(cv.players$size,cv.players$dev,type='b')

prune.players <- prune.tree(tree.players,best=12)
plot(prune.players)
text(prune.players,pretty=0)

# k) Predict “eur_value” in the test set using the regression tree model obtained in (j).
#Calculate the MSEs of the regression tree only using the test set. 

yhat=predict(prune.players,newdata=dataset2[-train.mat,])
boston.test=dataset2[-train.mat,"eur_value"]
RMSE4<-sqrt(mean((yhat-boston.test)^2))
RMSE4

#l) Fit random forests to predict “eur_value”. Use only the training set to fit the regression
#model. Determine the number of variables used in each split using the cross-validation.
#Grow 500 trees for random forest.

library(randomForest)
set.seed(1)


#10-fold cross validation
#rfcv(train[,1:37], train[,38], cv.fold=10, scale="log", step=0.5,  mtry=function(p) max(1, floor(sqrt(p))), recursive=FALSE)
#rf.players <- randomForest(eur_value~., data=train, mtry= i, ntree=500, importance=TRUE, na.action = na.omit)

cvplayer <- train[sample(nrow(train)),]
folds <- cut(seq(1,nrow(cvplayer)),breaks=5,labels=FALSE)

total_mse <- rep(NA,31)
for (i in 1:30) {
  mse <- rep(NA,5)
  #5-fold cross validation
  for (t in 1:5){
    set.seed(1)
    # cv_train_index <- sample(1:500,400)
    cv_test_index <- which(folds==t,arr.ind=TRUE)
    cv_train <- train[-cv_test_index,]
    cv_test <- train[cv_test_index,]
    rf.players <- randomForest(eur_value~., data=train, mtry= i, ntree=500, importance=TRUE, na.action = na.omit)
    pred <- predict(rf.players,newdata=cv_test)
    mse[t] <- (1/nrow(cv_test))*sum((pred-cv_test$eur_value)^2)
  }
  total_mse[i] <- mean(mse)
}

# Random Forest Code Without Cross Validation Code
#rf.players <- randomForest(eur_value~., data=train, mtry= 18, ntree=500, importance=TRUE, na.action = na.omit)

min_mtry <- which.min(total_mse)
min_mtry


# m) According to random forests, which variables are import? Comment.

importance(rf.players)
varImpPlot(rf.players, type=1)
varImpPlot(rf.players) 

#n) Predict “eur_value” in the test set using the random forest model obtained in (l).
#Calculate the MSEs of the random forest only using the test set.

set.seed(1)
rf.players <- randomForest(eur_value~., data=train, mtry= min_mtry, ntree=500, importance=TRUE, na.action = na.omit)
yhat.rf <- predict(rf.players,newdata=test)

RMSE5 <- sqrt(mean((yhat.rf-test$eur_value)^2))
RMSE5

#o) Compare MSEs obtained in (f), (i), (k) and (n)
c(RMSE, RMSE2, RMSE3, RMSE4, RMSE5)

