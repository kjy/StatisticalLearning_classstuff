### Week2 Chapter 3 Statistical Learning

library(MASS)  #has the datasets
library(ISLR)  # has datasets that are used in the book
### Simple linear regression
names(Boston)   #Boston dataset, names of variables
[1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"     "rad"    
[10] "tax"     "ptratio" "black"   "lstat"   "medv"

?Boston    #Gives description of the dataset, The Boston data frame has 506 rows and 14 columns
plot(medv~lstat,Boston)    #(outcome)medv = median value of owner-occupied homes in \$1000s.  (predictor)lstat=lower status of the population (percent).

fit1=lm(medv~lstat,data=Boston)  # ~ means "as modeled as"
fit1
Call:
lm(formula = medv ~ lstat, data = Boston)

Coefficients:
(Intercept)        lstat  
      34.55        -0.95    #coefficient shows negative relationship
      
summary(fit1)
Call:
lm(formula = medv ~ lstat, data = Boston)  

Residuals:
    Min      1Q  Median      3Q     Max 
-15.168  -3.990  -1.318   2.034  24.500 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 34.55384    0.56263   61.41   <2e-16 ***
lstat       -0.95005    0.03873  -24.53   <2e-16 ***  #statistically significant
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.216 on 504 degrees of freedom
Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16


abline(fit1,col="red")
names(fit1)
[1] "coefficients"  "residuals"     "effects"       "rank"          "fitted.values"
 [6] "assign"        "qr"    

confint(fit1)   #Gives the confidence intervals 
                2.5 %     97.5 %  #This is the 95% confidence interval
(Intercept) 33.448457 35.6592247
lstat       -1.026148 -0.8739505


predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")  #Predict function is a method to query a linear model fit
       fit      lwr      upr      #Query the 3 values (5, 10, 15) and get predicted values and confidence interval for each
1 29.80359 29.00741 30.59978
2 25.05335 24.47413 25.63256
3 20.30310 19.73159 20.87461



### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)  #2 predictors (lstat and age), outcome is medv
summary(fit2)
Call:
lm(formula = medv ~ lstat + age, data = Boston)

Residuals:
    Min      1Q  Median      3Q     Max 
-15.981  -3.978  -1.283   1.968  23.158 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
lstat       -1.03207    0.04819 -21.416  < 2e-16 ***  #Statistically significant
age          0.03454    0.01223   2.826  0.00491 **   #Statistically significant but less so than lstat
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.173 on 503 degrees of freedom
Multiple R-squared:  0.5513,	Adjusted R-squared:  0.5495 #R-squared is the percentage of variance explained
F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16


fit3=lm(medv~.,Boston)  #Use all other variables in the dataset, except medv, which is the outcome/response variable
summary(fit3)
Call:
lm(formula = medv ~ ., data = Boston)    # Look at the call to the function--a model with all the predictors

Residuals:
    Min      1Q  Median      3Q     Max 
-15.595  -2.730  -0.518   1.777  26.199 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
zn           4.642e-02  1.373e-02   3.382 0.000778 ***
indus        2.056e-02  6.150e-02   0.334 0.738288    
chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
age          6.922e-04  1.321e-02   0.052 0.958229     #Age no longer statistically significant  
dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
black        9.312e-03  2.686e-03   3.467 0.000573 ***
lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.745 on 492 degrees of freedom
Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16


par(mfrow=c(2,2))  #Make a 2x2 layout for the plot, 
plot(fit3)         #plot residuals against fitted values, take a look at the variance, look for non-linearities

fit4=update(fit3,~.-age-indus)  #update function, use model in fit3 but remove age and indus
summary(fit4)                   #indus = proportion of non-retail business acres per town.
Call:
lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
    tax + ptratio + black + lstat, data = Boston)

Residuals:
     Min       1Q   Median       3Q      Max 
-15.5984  -2.7386  -0.5046   1.7273  26.2373 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  36.341145   5.067492   7.171 2.73e-12 ***
crim         -0.108413   0.032779  -3.307 0.001010 ** 
zn            0.045845   0.013523   3.390 0.000754 ***
chas          2.718716   0.854240   3.183 0.001551 ** 
nox         -17.376023   3.535243  -4.915 1.21e-06 ***
rm            3.801579   0.406316   9.356  < 2e-16 ***    #Everything is significant in model
dis          -1.492711   0.185731  -8.037 6.84e-15 ***
rad           0.299608   0.063402   4.726 3.00e-06 ***
tax          -0.011778   0.003372  -3.493 0.000521 ***
ptratio      -0.946525   0.129066  -7.334 9.24e-13 ***
black         0.009291   0.002674   3.475 0.000557 ***
lstat        -0.522553   0.047424 -11.019  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.736 on 494 degrees of freedom
Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7348 
F-statistic: 128.2 on 11 and 494 DF,  p-value: < 2.2e-16


### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)   #Use interaction term (lstat*age)
summary(fit5)                    #Model will include 3 predictors (lstat, age, and lstat*age)
Call:
lm(formula = medv ~ lstat * age, data = Boston)  

Residuals:
    Min      1Q  Median      3Q     Max 
-15.806  -4.045  -1.333   2.085  27.552 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***   #main effect (statistically significant)
age         -0.0007209  0.0198792  -0.036   0.9711       #main effect (not statistically significant)
lstat:age    0.0041560  0.0018518   2.244   0.0252 *     #interaction effect (denoted by colon)
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.149 on 502 degrees of freedom
Multiple R-squared:  0.5557,	Adjusted R-squared:  0.5531 
F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

#Use a quadratic term since scatterplot of medv and lstat looked nonlinear
fit6=lm(medv~lstat +I(lstat^2),Boston); summary(fit6) #Need to put quadratic term inside the Identity function I() 

Call:
lm(formula = medv ~ lstat + I(lstat^2), data = Boston)  #Notice that Identity function I() shows up in the call

Residuals:
     Min       1Q   Median       3Q      Max 
-15.2834  -3.8313  -0.5295   2.3095  25.4148 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 42.862007   0.872084   49.15   <2e-16 ***
lstat       -2.332821   0.123803  -18.84   <2e-16 ***  #Linear is significant
I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***  #quadratic is significant
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.524 on 503 degrees of freedom
Multiple R-squared:  0.6407,	Adjusted R-squared:  0.6393 
F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16


attach(Boston)  #Named variables are now available in workspace

par(mfrow=c(1,1))  #Make a 1x1 layout
plot(medv~lstat)   #plot as before the same 2 variables, medv is outcome and lstat is predictor
#Now, include the fitted values of the quadratic fit, fit6, and put in the plot 
#Do not use abline since that is used for a straight line
#For each value of lstat, you get a fitted value from the model fit6
points(lstat,fitted(fit6),col="red",pch=20)  #Include the quadratic fit with a series of points 
                                             

#Use poly function to fit a polynomial 
fit7=lm(medv~poly(lstat,4))  #4th degree polynomial, non-linear in the variable yet linear in the coefficient
points(lstat,fitted(fit7),col="blue",pch=20)  #pch means plotting character, 20 is a round ball symbol

#To see all the plotting characters available to you
plot(1:20,1:20,pch=1:20,cex=2) #cex = 2 means double the size of the plotting character


###Qualitative predictors
fix(Carseats)  #pch means plotting character, 20 is a round ball  #Throws up a dataframe so that you can see it
names(Carseats)  #show all the variable names
[1] "Sales"       "CompPrice"   "Income"      "Advertising" "Population"  "Price"      
 [7] "ShelveLoc"   "Age"         "Education"   "Urban"       "US"   
 
summary(Carseats)
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
Call:
lm(formula = Sales ~ . + Income:Advertising + Age:Price, data = Carseats)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.9208 -0.7503  0.0177  0.6754  3.3413 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)         6.5755654  1.0087470   6.519 2.22e-10 ***
CompPrice           0.0929371  0.0041183  22.567  < 2e-16 ***
Income              0.0108940  0.0026044   4.183 3.57e-05 ***
Advertising         0.0702462  0.0226091   3.107 0.002030 ** 
Population          0.0001592  0.0003679   0.433 0.665330    
Price              -0.1008064  0.0074399 -13.549  < 2e-16 ***
ShelveLocGood       4.8486762  0.1528378  31.724  < 2e-16 ***
ShelveLocMedium     1.9532620  0.1257682  15.531  < 2e-16 ***
Age                -0.0579466  0.0159506  -3.633 0.000318 ***
Education          -0.0208525  0.0196131  -1.063 0.288361    
UrbanYes            0.1401597  0.1124019   1.247 0.213171    
USYes              -0.1575571  0.1489234  -1.058 0.290729    
Income:Advertising  0.0007510  0.0002784   2.698 0.007290 ** 
Price:Age           0.0001068  0.0001333   0.801 0.423812    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.011 on 386 degrees of freedom
Multiple R-squared:  0.8761,	Adjusted R-squared:  0.8719 
F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

contrasts(Carseats$ShelveLoc)  #ShelveLoc is a qualitative variable
                               #Contrast function will show you how R will code it 
       Good Medium             #3 level factor means 2 dummy variables need to be made
Bad       0      0             
Good      1      0
Medium    0      1

###Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)      #Make the Carseats data available in the data space
regplot(Price,Sales)  #Call the regplot function with Price, Sales as variables passed in as arguments

regplot=function(x,y,...){     #. . . means there are unnamed arguments, can later add extra arguments (regplot call below)
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

##QUESTION  What is the difference between lm(y ~ x*z) and lm(y ~ I(x*z)), when x and z are both numeric variables?  See examples below.
lm(medv~lstat*age) gives interaction effects and main effects
lm(medv~I(lstat*age)) gives interaction effects only


> fit15 = lm(medv~lstat*age)
> summary(fit15)

Call:
lm(formula = medv ~ lstat * age)

Residuals:
    Min      1Q  Median      3Q     Max 
-15.806  -4.045  -1.333   2.085  27.552 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***  #main effects
age         -0.0007209  0.0198792  -0.036   0.9711      #main effects
lstat:age    0.0041560  0.0018518   2.244   0.0252 *    #interaction effects
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.149 on 502 degrees of freedom
Multiple R-squared:  0.5557,	Adjusted R-squared:  0.5531 
F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

> fit16 = lm(medv~I(lstat*age))
> summary(fit16)

Call:
lm(formula = medv ~ I(lstat * age))

Residuals:
    Min      1Q  Median      3Q     Max 
-13.347  -4.372  -1.534   1.914  27.193 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)    30.1588631  0.4828240   62.46   <2e-16 ***
I(lstat * age) -0.0077146  0.0003799  -20.31   <2e-16 ***  #Interaction effects only
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.827 on 504 degrees of freedom
Multiple R-squared:  0.4501,	Adjusted R-squared:  0.449 
F-statistic: 412.4 on 1 and 504 DF,  p-value: < 2.2e-16





