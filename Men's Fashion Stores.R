library (faraway)
library (MASS)
library (nlme)

library(Ecdat)
data(Clothing)
attach(Clothing)

fit = lm(sqrt(tsales)~. , Clothing)
summary(fit)

plot(fit)



jack = rstudent (fit)
jack[which.max(abs(jack))]
qt(.05/(400*2), 412)
### 8.74 > 3.87, therefore is a outliner #391

cook = cooks.distance(fit)
halfnorm(cook,4, labs =names(cook), ylab = "Cook's Distance")
## influencetial points are 397, 391, 271, 268

new.data = Clothing[-c(397,391,271, 268, 233, 128),] 

par(mfrow=c(1,2))
plot(fitted(fit),residuals(fit),xlab="Fitted", ylab="Residuals", main= "Fitted vs Residuals")
abline(h=0)
plot(fitted(fit), abs(residuals(fit)),xlab="Fitted", ylab= "|Residuals|")
par(mfrow = c(1,1))
## The Fitted vs Residual suggest the variance is heteroscedastic.

qqnorm(residuals(fit), ylab = "Residuals", main ="Clothing Data")
qqline(residuals(fit))

shapiro.test(residuals(fit))

## The QQ plot and shapiro with small p-value suggest the variance are not normal. 
## Transformation is needed. 

## Independence 
plot(residuals(fit), ylab = "Residuals", main = "Index Plot")
abline(h=0)

library(lmtest)
dwtest(tsales~. , data = Clothing)
## The Index plot show that there is no correlated error and the p-value of the Durbin-Wastson Test confirms there is not autocorrelation. 

## Box-Cox method
library(MASS)
par(mfrow = c(1,2))
boxcox(fit, plotit =T)
boxcox(fit, plotit =T, lambda = seq(0.2,.8,by=.1))
par(mfrow = c(1,1))
## The Box-Cox show that the lambda with 95% confidence interval is [ 0.32, 0.48]. We choose a lambda that is close to .48 whcih is .5. so, we perform a sqaure root transformation. 




## Model with transforamtion and with outliners removed. 
md = lm(sqrt(tsales) ~., new.data)
summary(md)
plot(md)


## Variable Selection using Mallow’s Cp statistic
library(leaps)
all<- regsubsets((tsales) ~., new.data)
(rs<-summary(all))

par(mfrow=c(1,2))
plot(2:9,rs$adjr2, xlab="No.of Parameters", ylab="Adjusted R-square")
plot(2:9,rs$cp, xlab="No. of Parameters", ylab="C_p Statistics")
abline(0,1)
par(mfrow=c(1,1))


md1 = step(md)
summary(md1)

md3 = lm(sqrt(tsales) ~ sales + margin + + npart + naux + hourspw + ssize, new.data)
anova(md3, md1)
summary(md3)
par(mfrow=c(2,2))
plot(md3)
par(mfrow=c(1,1))










