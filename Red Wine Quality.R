winequality.white <- read.csv("~/Desktop/winequality-white.txt", sep=";")
white = winequality.white

library(faraway)
summary(white)
attach(white)

par(mfrow = c(1,6))
hist(alcohol)
hist(chlorides)
hist(citric.acid)
hist(density)
hist(fixed.acidity)
hist(free.sulfur.dioxide)
hist(pH)
hist(quality)
hist(residual.sugar) 
hist(sulphates)
hist(total.sulfur.dioxide)
hist(volatile.acidity)
par(mfrow = c(1,1))

par(mfrow = c(1,6))
boxplot(alcohol, main = "Boxplot of Alcochol")
boxplot(chlorides, main = "Boxplot of Chlorides")
boxplot(citric.acid,main = "Boxplot of Citric Acid")
boxplot(density,main = "Boxplot of Density")
boxplot(fixed.acidity,main = "Boxplot of Fixed Acidity")
boxplot(free.sulfur.dioxide,main = "Boxplot of Free Sulfur")
boxplot(pH, main = "Boxplot of pH")
boxplot(quality,main = "Boxplot of Quality")
boxplot(residual.sugar, main = "Boxplot of Residual Sugar")
boxplot(sulphates,main = "Boxplot of Sulphates")
boxplot(total.sulfur.dioxide,main = "Boxplot of Total Sulfur Dioxide")
boxplot(volatile.acidity,main = "Boxplot of Volatile Acidity")
par(mfrow = c(1,1))


pairs(white)

library(plyr)
white = rename(white, c('free.sulfur.dioxide' = 'free SO2', 'total.sulfur.dioxide' = 'total SO2' ))
round(cor(white),2)

## Fromt the boxplot, we see that there is possible outliners greater than Q3 + 1.5IQR. 
## Remove outliners from the dataset.

## Clean data
a = 0
for( i in 1:(ncol(white)-1)){
  a[i] = (quantile(white[,i],0.75) + 1.5*IQR(white[,i], 0.75))
}


index = matrix(0, nrow(white), ncol(white)-1)
for (i in 1:nrow(white))
  for(j in 1:(ncol(white)-1)){
    if(white[i,j] > a[j]) index[i,j] = 1}

tot = apply(index, 1, sum)

b = 0
j = 1
for(i in 1:nrow(white)) { if( tot[i]> 0){ b[j]=i   
                                          j=j+1} else j = j}

newdata = white[-b,]

#full model
fit = lm(quality~., data = newdata)
summary(fit)


fit1 = lm(quality~. -density, data = )
summary(fit1)

fit2 = step(fit1, trace = F)
summary(fit2)

## Outlier and influential points
plot(predict(fit), residuals(fit2))
identify(qqnorm(residuals(fit)))##[1]  206  371  620 1008 3122

library(faraway)
cook = cooks.distance(fit)
halfnorm(cook,13, labs =names(cook), ylab = "Cook's Distance", main = "Cook's Distance")
## 741,254,3266,446,1230,3771,3811,2374,868,4509,527

jack = rstudent(fit)
qt = abs((qt(.05/(4074*2),(4074+13-1))))
jack[which(abs(jack) >= qt)] ## 254, 446

remove = c(206,371,620,1008,3122,741,254,3266,446,1230,3771,3811,2374,868,4509,527)

fit2 = lm(quality~., data = newdata[-remove,])
summary(fit2)

##VIF
vif(newdata[-remove,])
vif(newdata[-remove,-c(8)])
fit3 = lm(quality~. -density, data = newdata[-remove,])

##Step function 
fit4 = step(fit3)
summary(fit4)


## Drop1()
drop1(fit4, test = "F")


##Final Model
final = update(fit4, ~.-free.sulfur.dioxide)
summary(final)

## proportiob of quality
round(prop.table(ftable(newdata$quality)),3)


##Assign factor of quality 
newdata$factorQ[newdata$quality < 6 ] = 'Low'
newdata$factorQ[newdata$quality == 6 ] = 'Medium'
newdata$factorQ[newdata$quality > 6 ] = 'High'
newdata$factorQ = factor(newdata$factorQ)
## sample 2/3 of data as training set and 1/3 of data as testing set

newdata = newdata[-remove,]
indexes = sample(1:nrow(newdata), (2/3)*nrow(newdata), replace = F)
training = newdata[indexes, -c(12)]
testing = newdata[-indexes, -c(12)]

# Classification Tree
library(rpart)
tree = rpart(factorQ ~., method = "class", training)
plot(tree,compress=T, uniform=T, branch= 0.4, margin = 0.1, main="Classification Tree for White Wines")
text(tree, all=TRUE, cex = .6)

printcp(tree)

yhat.tree = predict(tree, testing, type = "class")
(tree.table = table(yhat.tree, testing$factorQ))
(tree.mis = 1-sum(diag(tree.table))/sum(tree.table))

# Random Forest
library(randomForest)
forest = randomForest(factorQ ~., data = training, ntree = 100, importance = T, proximity =T)
print(forest)

yhat.forest = predict(forest, testing)
(forest.table = table(yhat.forest, testing$factorQ))
(forest.mis = 1-sum(diag(forest.table))/sum(forest.table))
plot(forest, main="Random Forest of White Wines")
varImpPlot(forest,  main="Important Variables Plot", cex=0.8)

# Boosting
library(bagRboostR)
yhat.bagging = bagging(factorQ~., training, test = testing)
bagging.table = table(yhat.bagging, testing$factorQ)
(bagging.mis = 1-sum(diag(bagging.table))/sum(bagging.table))

yhat.samme = samme(factorQ~., training, test = testing)
samme.table = table(yhat.samme, testing$factorQ)
(boosting.mis = 1-sum(diag(samme.table))/sum(samme.table))

library(gbm)
gbm = gbm(factorQ~., training, distribution = "multinomial")
pred = predict.gbm(gbm, testing, n.trees = 100)
?predict.gbm

##SVM
library(e1071)
svm1 = svm(factorQ~., training, cost = 100, gamma = 0.01)
plot(svm1, training, density~alcohol)
tuned <- tune.svm(factorQ~., data= training, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)

pred = predict(svm, testing)
(table = table(pred, testing$factorQ))
(1-sum(diag(table))/sum(table))

