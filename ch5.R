### Week 4 Chapter 5  Statistical Learning

#  LOOCV, K-Fold Cross-Validation, and the Bootstrap  (Resampling methods)
require(ISLR)  # require function is bool and will return false if pkg does not exist 
require(boot)  # Use the boot package
?cv.glm        # This function calculates the estimated K-fold cross-validation prediction error for generalized linear models.
plot(mpg~horsepower,data=Auto)   # As horsepower increases, mpg decreases

## LOOCV   (Leave One Out Cross-Validation)
glm.fit=glm(mpg~horsepower, data=Auto)  # Fits a linear model if family is not specified (can use glm)
# Fits a model repeatedly n times while leaving out 1 observation each time. Makes a fit on all other data.
# Makes a prediction on the x observation that was left out.
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)
[1] 24.23151 24.23114  
# Delta is vector of length two. The first component is the raw cross-validation estimate of prediction error. 
# The second component is the adjusted cross-validation estimate. 
# The adjustment is designed to compensate for the bias introduced by not using leave-one-out cross-validation.

##Lets write a simple function to use formula (5.2)  Misclassification error: 1/n* SUMMATION(yi - yhat(-i))^2
loocv=function(fit){                    # How much does i contribute to its own fit? A measure of self-influence of i.
  h=lm.influence(fit)$h                 # This is the formula computed:  1/n* SUMMATION(yi - yhati)^2/(1-Hii)^2
  mean((residuals(fit)/(1-h))^2)        # h = Hii is the diagonal of the hat matrix 
}                                       
## Now we try it out
loocv(glm.fit)
[1] 24.23151                                        

cv.error=rep(0,5)     #  Create vector to collect the errors 
degree=1:5            #  Now fit polynomials of different degrees to the data
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)                    # Compute error and put in error vector
}
plot(degree,cv.error,type="b")  # Plot error against degree. It shows quadratic does the best in terms of lowest error 

## 10-fold CV   Divide data into 10 parts. 1 part is test set. 9 parts is training set. Fits the model 10 times, switching out test set.

cv.error10=rep(0,5)           # Create a vector to accept the errors
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]   # Specify K = 10 for 10 folds
}
lines(degree,cv.error10,type="b",col="red")      # Similar story as before. 

# If given a choice, pick K-fold CV over LOOCV since it is cheaper to compute.



## Bootstrap--repeated, random sampling of training observations. Invented by Brad Efron.
## Some observations can be represented more than once and some not at all.
## Minimum risk investment - Section 5.2    2 investments called X and Y

alpha=function(x,y){                    # VAR(X), VAR(Y)
  vx=var(x)                             # alpha = VAR(Y) - COV(X,Y)/VAR(X)+VAR(Y)-2COV(X,Y)
  vy=var(y)                             # What is the standard error (sampling variablity) of alpha? Don't know. 
  cxy=cov(x,y)                          # Need bootstrap
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)         #  Portfolio is the dataset
[1] 0.5758321

## What is the standard error of alpha?
## Need a wrapper that allows the bootstrap to work. Takes a dataframe and an index (over the rows, 1 to n, can be repeats)
alpha.fn=function(data, index){       #  index tells you which observations gets represented in the sampling
  with(data[index,],alpha(X,Y))       #  with is a function that takes dataframe as 1st argument. It says
}                                     #  using this data in the dataframe, execute these commands.

alpha.fn(Portfolio,1:100)
[1] 0.5758321

#Now, run the bootstrap. Set the random number seed. 
set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))  #  Sample 1 to 100 with replacement
[1] 0.5963833    

boot.out=boot(Portfolio,alpha.fn,R=1000)  # Do a 1000 bootstraps
boot.out                                  # Gives a summary of the bootstrap
ORDINARY NONPARAMETRIC BOOTSTRAP


Call:
  boot(data = Portfolio, statistic = alpha.fn, R = 1000)


Bootstrap Statistics :
  original        bias    std. error       # Gives estimate, bias, and standard error
t1* 0.5758321 -7.315422e-05  0.08861826    

plot(boot.out)        # histogram (symmetrical distribution, Gaussian), QQ-plot (lines up as a straight line: Gaussian)

# The bootstrap can give you reliable estimates of standard error from nasty statistics.
