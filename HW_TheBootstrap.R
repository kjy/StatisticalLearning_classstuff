setwd("/Users/karenyang/Downloads/")
load("/Users/karenyang/Downloads/5.R.RData")
ls()
head(Xy)
tail(Xy)
dim(Xy)
[1] 1000    3

attach(Xy)
search()

# Q1. Download the file 5.R.RData and load it into R using load("5.R.RData"). 
# Consider the linear regression model of y on X1 and X2. 
# To within 10%, what is the standard error for β1? 0.026
model1 <- lm(y ~ X1 + X2)
summary(model1)
Call:
  lm(formula = y ~ X1 + X2)

Residuals:
  Min       1Q   Median       3Q      Max 
-1.44171 -0.25468 -0.01736  0.33081  1.45860 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.26583    0.01988  13.372  < 2e-16 ***
  X1           0.14533    0.02593   5.604 2.71e-08 ***
  X2           0.31337    0.02923  10.722  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5451 on 997 degrees of freedom
Multiple R-squared:  0.1171,  Adjusted R-squared:  0.1154 
F-statistic: 66.14 on 2 and 997 DF,  p-value: < 2.2e-16

# Q2 Next, plot the data using matplot(Xy,type="l"). 
# Which of the following do you think is most likely given what you see? Our estimate of s.e.(β^1) is too low. 
?matplot
matplot(Xy, type="l")

# Q3 Now, use the (standard) bootstrap to estimate s.e.(β^1). To within 10%, what do you get?

library(boot)
alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy= cov(x,y)
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Xy$X1,Xy$y)
[1] 0.4167192
alpha.fn = function(data,index){
  with(data[index,],alpha(Xy$X1,Xy$y))
}

alpha.fn<-function(data, index) {
  fit1<-lm(y~., data=Xy[index,])
  coefficients(fit1)[['X1']]
}

set.seed(1)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))
[1] 0.1059068

boot.out=boot(Xy,alpha.fn,R=1000)
boot.out

ORDINARY NONPARAMETRIC BOOTSTRAP


Call:
  boot(data = Xy, statistic = alpha.fn, R = 1000)


Bootstrap Statistics :
  original       bias    std. error
t1* 0.1453263 0.0001885914  0.02873965

# Q4. Finally, use the block bootstrap to estimate s.e.(β^1). Use blocks of size 100. To within 10%, what do you get?
?tsboot
# Generate R bootstrap replicates of a statistic applied to a time series. 
# The replicate time series can be generated using fixed or random block lengths 
# or can be model based replicates.


tsboot.out = tsboot(Xy, se_stat, R = 1000, l = 100, sim = "fixed")
tsboot.out
