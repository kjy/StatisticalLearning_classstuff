

load("/Users/karenyang/Desktop/Statistical_Learning_Rcode/10.R.RData")
ls()
[1] "x"      "x.test" "y"      "y.test"


Suppose we want to fit a linear regression, but the number of variables is much larger than the number of observations. In some cases, we may improve the fit by reducing the dimension of the features before.
In this problem, we use a data set with n = 300 and p = 200, so we have more observations than variables, but not by much. Load the data x, y, x.test, and y.test from 10.R.RData.
First, concatenate x and x.test using the rbind functions and perform a principal components analysis on the concatenated data frame. 
Q.1 To within 10% relative error, what proportion of the variance is explained by the first five principal components?
xvars = rbind(x,x.test)  # bind the variables

dataset1 = data.frame(xvars)  #create a dataset

pca.out = prcomp(dataset1, scale = TRUE) # prinicipal component analysis model
pca.out$sdev   # gives standard deviations 

screeplot(pca.out)   # Scree plot shows variance explained per principal component
(pca.out$sdev)^2/ sum(pca.out$sdev^2)
sum(0.1278392623, 0.1056409183, 0.0693007523, 0.0363725007, 0.0107030317)  #Take sum of first five
[1] 0.3498565 #cumulative sum
#alternative method is to use the cumulative sum function, which gives the same 0.3498565 on fifth entry
cumsum((pca.out$sdev)^2) / sum(pca.out$sdev^2)
[1] 0.1278393 0.2334802 0.3027809 0.3391534 0.3498565

#or just use
summary(pca.out)
Importance of components:
                          PC1    PC2    PC3     PC4    PC5     PC6     PC7     PC8     PC9
Standard deviation     5.0565 4.5965 3.7229 2.69713 1.4631 1.16827 1.15848 1.15544 1.14591
Proportion of Variance 0.1278 0.1056 0.0693 0.03637 0.0107 0.00682 0.00671 0.00668 0.00657
Cumulative Proportion  0.1278 0.2335 0.3028 0.33915 0.3499 0.35668 0.36339 0.37007 0.37663


#Q.2 The previous answer suggests that a relatively small number of '"latent variables" account for a substantial fraction of the features' variability. We might believe that these 
latent variables are more important than linear combinations of the features that have low variance.
We can try forgetting about the raw features and using the first five principal components (computed on rbind(x,x.test)) instead as low-dimensional derived features. What is the mean-squared test error if we regress y on the first five principal components, and use the resulting model to predict y.test? 
xols<-pca.out$x[1:300,1:5]
fit0 <- lm(y ~ xols)
summary(fit0)
Call:
lm(formula = y ~ xols)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.3289 -0.6992  0.0319  0.8075  2.5240 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.09541    0.06107   1.562 0.119314    
xolsPC1      0.07608    0.01159   6.564 2.36e-10 ***
xolsPC2     -0.02276    0.01314  -1.732 0.084309 .  
xolsPC3     -0.04023    0.01538  -2.616 0.009352 ** 
xolsPC4     -0.06368    0.02237  -2.847 0.004722 ** 
xolsPC5     -0.16069    0.04299  -3.738 0.000223 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.056 on 294 degrees of freedom
Multiple R-squared:  0.1906,	Adjusted R-squared:  0.1769 
F-statistic: 13.85 on 5 and 294 DF,  p-value: 3.704e-12

yhat0 = predict(fit0, x.test)
mean((yhat0-y.test)^2)

[1] 1.40799 #not correct
[1] 1.413063 #not correct




# Q.3 Now, try an OLS linear regression of y on the matrix x. What is the mean squared prediction error if we use the fitted model to predict y.test from x.test?
fit<-lm(y~.,x)  # Run linear model
summary(fit)
yhat = predict(fit, newdata=x.test)  # Use linear model and new dataset to get the predicted values, yhat
mean((yhat-y.test)^2)  #Calculate mean squared error (difference between predicted y and true y values is error)
[1] 3.657197           #Grader shows answer to be 3.90714 