In this problem, you will use simulation to evaluate (by Monte Carlo) the expected misclassification error rate given a particular generating model.  Let yi be equally divided between classes 0 and 1, and let xi ∈ ℝ10 be normally distributed. Given yi=0, xi ∼ N10(0, I10).  Given yi=1, xi ∼N10(μ, I10) with μ=(1,1,1,1,1,0,0,0,0,0).
Now, we would like to know the expected test error rate if we fit an SVM to a sample of 50 random training points from class 1 and 50 more from class 0.  We can calculate this to high precision by 1) generating a random training sample to train on, 2) evaluating the number of mistakes we make on a large test set, and then 3) repeating (1-2) many times and averaging the error rate for each trial.

install.packages("MASS")  # Use this package for the mvrnorm() function
library(MASS)

install.packages("e1071") # Use this package for the SVM() function
library(e1071)

#Create a dataset of 100 observations (with 50 1's and 50 0's) with 10 predictors
set.seed(10111) 
# n is number of obs., mu is vector mean, sigma is Identity matrix (variance-covariance)
x1 = mvrnorm(n = 50, mu = rep(0,10), Sigma=diag(10)) 
dim(x1)
[1] 50 10
x2 = mvrnorm(n = 50, mu = rep(0,10), Sigma=diag(10)) 
xvar_1 = rbind(x1,x2)
dim(xvar_1)
[1] 100  10
data1 = matrix(xvar_1)
y = rep(c(0,1), c(50,50)) #Make a vector of 100 obs. with 50 1's and 50 0's
y = matrix(y)
dim (y)
#Put the y variable in with the X variables to form a training dataset
train_data = data.frame(xvar_1, y = as.factor(y))
dim (train_data)
[1] 100  11

#Create a large dataset with 1000 observations for the test datset with 10 predictors
x3 = mvrnorm(500,rep(0,10),diag(10))
x4 = mvrnorm(500,rep(c(1,0),c(5,5)),diag(10))
xvar_2 = rbind(x3,x4)
y1 = rep(c(0,1),c(500,500))
test_data = data.frame(xvar_2,y1=as.factor(y1))
dim (test_data)
[1] 1000   11  #Test data set with 1000 observations, Y variable with 500 1's and 500 0's, and 10 X variables


# Q9.1 Use svm in the e1071 package with the default settings (the default kernel is a radial kernel). What is the expected test error rate of this method (to within 10%)?
sample1_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
svmModel1 = svm(sample1_train$y ~., data=sample1_train)
svmModel1
svmPredicted1 <- predict(svmModel1, newdata = test_data)
svmPredicted1
errorrateSVM1 <- (sum(svmPredicted1 != test_data$y))/length(test_data$y)
errorrateSVM1
[1] 0.421


sample2_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
svmModel2 = svm(sample2_train$y ~., data=sample2_train)
svmModel2
svmPredicted2 <- predict(svmModel2, newdata = test_data)
svmPredicted2
errorrateSVM2 <- (sum(svmPredicted2 != test_data$y))/length(test_data$y)
errorrateSVM2
[1] 0.465


sample3_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
svmModel3 = svm(sample2_train$y ~., data=sample3_train)
svmModel3
svmPredicted3 <- predict(svmModel3, newdata = test_data)
svmPredicted3
errorrateSVM3 <- (sum(svmPredicted3 != test_data$y))/length(test_data$y)
errorrateSVM3
[1] 0.476
#Take the average of the test errors for SVM 
sum(errorrateSVM1, errorrateSVM2, errorrateSVM3)/3
[1] 0.454

#USe Monte Carlo method to repeat above for 1000 iterations on SVM with default setting radial kernel
error_vector <- c();
for (i in 1:1000) {
  ## Generate traning data
  sample_train <- train_data[sample(1:nrow(train_data), 50, replace = TRUE),]

  ## Fit a model on trainning data
  svmModel = svm(sample_train$y ~., data=sample_train)

  ## Generate test data
  #Use test_data from above
  
  ## Predict on test  data
  svmPredicted <- predict(svmModel, newdata = test_data)
  
  ## Compare prediction with y
  errorrateSVM <- (sum(svmPredicted != test_data$y))/length(test_data$y)  
  
  ## Save the result in a vector
  error_vector <- c(error_vector, errorrateSVM)
  
}

#Random check the values in the vector
> head(error_vector)
[1] 0.620 0.604 0.534 0.492 0.599 0.526

error_vector[999]
[1] 0.511

error_vector[750]
[1] 0.526

# Take the average across the 1000 test errors
mean(error_vector)
[1] 0.513473


# Q.9.2 Now fit an svm with a linear kernel (kernel = "linear"). What is the expected test error rate to within 10%?
sample6_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
svmModel6 = svm(sample6_train$y ~., data=sample6_train, kernel = "linear")
svmModel6
svmPredicted6 <- predict(svmModel6, newdata = test_data)
svmPredicted6
errorrateSVM6 <- (sum(svmPredicted6 != test_data$y))/length(test_data$y)
errorrateSVM6
[1] 0.451

sample7_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
svmModel7 = svm(sample7_train$y ~., data=sample7_train, kernel = "linear")
svmModel7
svmPredicted7 <- predict(svmModel7, newdata = test_data)
svmPredicted7
errorrateSVM7 <- (sum(svmPredicted7 != test_data$y))/length(test_data$y)
errorrateSVM7
[1] 0.537

#Take the average of the test errors for SVM models with linear kernel
sum(errorrateSVM6, errorrateSVM7)/2
[1] 0.494

#USe Monte Carlo method to repeat above for 1000 iterations on SVM with linear kernel
error_vector1 <- c();
for (i in 1:1000) {
  ## Generate traning data
  sample_train <- train_data[sample(1:nrow(train_data), 50, replace = TRUE),]

  ## Fit a model on trainning data
  svmModel = svm(sample_train$y ~., data=sample_train, kernel = "linear")

  ## Generate test data
  #Use test_data from above
  
  ## Predict on test  data
  svmPredicted <- predict(svmModel, newdata = test_data)
  
  ## Compare prediction with y
  errorrateSVM <- (sum(svmPredicted != test_data$y))/length(test_data$y)  
  
  ## Save the result in a vector
  error_vector1 <- c(error_vector1, errorrateSVM)
  
}

# Random check the values in the vector
head(error_vector1)
[1] 0.553 0.608 0.461 0.483 0.542 0.469

error_vector1[999]
[1] 0.382

error_vector1[750]
[1] 0.415

# Take the average across the 1000 test errors
mean(error_vector1)
[1] 0.483152


# Q. 9.3 What is the expected test error for logistic regression? (to within 10%)

sample11_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]

logistic1 = glm(sample11_train$y ~., family = "binomial", data=sample11_train)
summary(logistic1)                          
fitted <- predict(logistic1, newdata=test_data, type = 'response')
missClass <- function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
errorratelog1 <-missClass(test_data$y,fitted)
[1] 0.576


sample12_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
logistic2 = glm(sample12_train$y ~., family = "binomial", data=sample12_train)
summary(logistic2)                          
fitted1 <- predict(logistic2, newdata=test_data, type = 'response')
missClass <- function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
errorratelog2 <- missClass(test_data$y,fitted1)                        
[1] 0.561

sample13_train <- train_data[sample(1:nrow(train_data), 50,
                          replace=TRUE),]
logistic3 = glm(sample13_train$y ~., family = "binomial", data=sample13_train)
summary(logistic3)                          
fitted2 <- predict(logistic3, newdata=test_data, type = 'response')
missClass <- function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
errorratelog3 <- missClass(test_data$y,fitted2) 
[1] 0.425
#Take the average of the test errors for binary logistic
sum(errorratelog1, errorratelog2, errorratelog3)/3



#USe Monte Carlo method to repeat above for 1000 iterations on binary logistic regression model

#Use a function for missclassification error rate
missClass <- function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
#Set up a vector to store the error rates 
error_vector2 <- c();

for (i in 1:1000) {
  ## Generate traning data
  sample_train <- train_data[sample(1:nrow(train_data), 50, replace = TRUE),]

  ## Fit a model on trainning data
  LogisticModel = glm(sample_train$y ~., family = "binomial", data=sample_train)
 
  ## Generate test data
  #Use test_data from above
  
  ## Predict on test  data
  fitted <- predict(LogisticModel, newdata=test_data, type = 'response')
  
  ## Compare prediction with y
  errorrate_logistic <- missClass(test_data$y,fitted) 
  
  ## Save the result in a vector
  error_vector2 <- c(error_vector2, errorrate_logistic)
  
}
#(Don't worry if you get errors saying the logistic regression did not converge.)Warning messages:
1: glm.fit: algorithm did not converge 
2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
3: glm.fit: fitted probabilities numerically 0 or 1 occurred 
4: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# Random check the values in the vector
head(error_vector2)
[1] 0.444 0.280 0.419 0.593 0.501 0.686
error_vector2[999]
[1] 0.419
error_vector2[750]
[1] 0.548

# Take the average across the 1000 test errors
mean(error_vector2)
[1] 0.476648
