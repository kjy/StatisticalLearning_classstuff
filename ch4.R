### Week 3 Chapter 4 Statistical Learning

require(ISLR)  #require command is similar to libary command; load library, has the dataset on Stock Market
names(Smarket) #gives names of variables; Direction is the response(outcome) variable
[1] "Year"      "Lag1"      "Lag2"      "Lag3"      "Lag4"      "Lag5"      "Volume"   
[8] "Today"     "Direction"

summary(Smarket)
      Year           Lag1                Lag2                Lag3          
 Min.   :2001   Min.   :-4.922000   Min.   :-4.922000   Min.   :-4.922000  
 1st Qu.:2002   1st Qu.:-0.639500   1st Qu.:-0.639500   1st Qu.:-0.640000  
 Median :2003   Median : 0.039000   Median : 0.039000   Median : 0.038500  
 Mean   :2003   Mean   : 0.003834   Mean   : 0.003919   Mean   : 0.001716  
 3rd Qu.:2004   3rd Qu.: 0.596750   3rd Qu.: 0.596750   3rd Qu.: 0.596750  
 Max.   :2005   Max.   : 5.733000   Max.   : 5.733000   Max.   : 5.733000  
      Lag4                Lag5              Volume           Today           Direction 
 Min.   :-4.922000   Min.   :-4.92200   Min.   :0.3561   Min.   :-4.922000   Down:602  
 1st Qu.:-0.640000   1st Qu.:-0.64000   1st Qu.:1.2574   1st Qu.:-0.639500   Up  :648  
 Median : 0.038500   Median : 0.03850   Median :1.4229   Median : 0.038500             
 Mean   : 0.001636   Mean   : 0.00561   Mean   :1.4783   Mean   : 0.003138             
 3rd Qu.: 0.596750   3rd Qu.: 0.59700   3rd Qu.:1.6417   3rd Qu.: 0.596750             
 Max.   : 5.733000   Max.   : 5.73300   Max.   :3.1525   Max.   : 5.733000    
 
 #Lag1-Lag5 gives previous day's price
 
?Smarket  #  Daily percentage returns for the S&P 500 stock index between 2001 and 2005.

pairs(Smarket,col=Smarket$Direction)  #Create a matrix of scatterplots and make color contrast for the binary classes
# Binary Logistic regression model, using Direction as outcome variable
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)  #Gives summary output
Call:
glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + 
    Volume, family = binomial, data = Smarket)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-1.446  -1.203   1.065   1.145   1.326  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)
(Intercept) -0.126000   0.240736  -0.523    0.601
Lag1        -0.073074   0.050167  -1.457    0.145
Lag2        -0.042301   0.050086  -0.845    0.398    #Nothing is significant in model
Lag3         0.011085   0.049939   0.222    0.824
Lag4         0.009359   0.049974   0.187    0.851
Lag5         0.010313   0.049511   0.208    0.835
Volume       0.135441   0.158360   0.855    0.392

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1731.2  on 1249  degrees of freedom   #Null deviance is the deviance of mean
Residual deviance: 1727.6  on 1243  degrees of freedom   #Deviance of residuals
AIC: 1741.6

Number of Fisher Scoring iterations: 3


glm.probs=predict(glm.fit,type="response")   #Give predicted probabilities with command type = "response"
glm.probs[1:5]                               #Show the 1st five observations (rows) of predicted values
        1         2         3         4         5 
0.5070841 0.4814679 0.4811388 0.5152224 0.5107812 

glm.pred=ifelse(glm.probs>0.5,"Up","Down")   #Set threshold at 0.5. If True that > 0.5, then "Up"; else, "Down"

attach(Smarket)
table(glm.pred,Direction)  #glm.pred is the predicted values while Direction is the true values

        Direction     #This is a Confusion Matrix
glm.pred Down  Up
    Down  145 141    #  The diagonal values are the correct classifications
    Up    457 507    #  The off-diagonal values are the incorrect classifications
    
mean(glm.pred==Direction)  #This gives the classification rate of model
[1] 0.5216           #   Performed slightly better than chance (0.50)



# Make training and test set
train = Year<2005   #Create training data set
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial, subset=train)  #The subset = train where train is < 2005

#Use the remaining !train for the test data set           
glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") type = "response" #gives predicted probabilities

glm.pred=ifelse(glm.probs >0.5,"Up","Down")  #Make an up/down variable, if true, then "Up", else false, then "Down"
Direction.2005=Smarket$Direction[!train]     #Make a variable of the true values

table(glm.pred,Direction.2005)   #Give table of classification of test data set with predicted against true values     
Direction.2005
glm.pred Down Up      #This is a Confusion Matrix
    Down   77 97      #Off-diagonal values are the mistakes
    Up     34 44      #Diagonal values are where we got it right
    
mean(glm.pred==Direction.2005)   #Classification rate
[1] 0.4801587     #The mean is less than 50%, which is the null rate, which could be overfitting of model


#Fit smaller model (only include 2 predictors, lag1 and lag2)
glm.fit=glm(Direction~Lag1+Lag2,
            data=Smarket,family=binomial, subset=train)
summary(glm.fit)
Call:
glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = Smarket, 
    subset = train)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-1.345  -1.188   1.074   1.164   1.326  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)
(Intercept)  0.03222    0.06338   0.508    0.611
Lag1        -0.05562    0.05171  -1.076    0.282
Lag2        -0.04449    0.05166  -0.861    0.389

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1383.3  on 997  degrees of freedom     #Deviance of the mean
Residual deviance: 1381.4  on 995  degrees of freedom     #Residual deviance
AIC: 1387.4

Number of Fisher Scoring iterations: 3

glm.probs=predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred=ifelse(glm.probs >0.5,"Up","Down")
table(glm.pred,Direction.2005)

        Direction.2005          #Confusion Matrix
glm.pred Down  Up               
    Down   35  35               #Correct classifications are on the diagonal
    Up     76 106               #Incorrect classifcations are on the off-diagonal
mean(glm.pred==Direction.2005)   #This is the classification rate of model
[1] 0.5595238     #Mean shows a better classification rate than previous dataset

106/(76+106)
[1] 0.5824176  #Correct classification rate for "Up" when predicting "Up" is 0.58



require (ISLR)  # This has the dataset on the Stock market
require(MASS)   # This has the Linear Discriminant Analysis commands that we need
## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit
Call:
lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = Year < 
    2005)

Prior probabilities of groups:    #Gives proportions of Up and Down in dataset
    Down       Up 
0.491984 0.508016 

Group means:                    #Summary of group means
            Lag1        Lag2
Down  0.04279022  0.03389409
Up   -0.03954635 -0.03132544

Coefficients of linear discriminants:   #LDA coefficients
            LD1
Lag1 -0.6420190
Lag2 -0.5135293

plot(lda.fit)
#Create a test dataset
Smarket.2005=subset(Smarket,Year==2005)  #dataframe is Smarket, subset out the observations with Year is 2005
lda.pred=predict(lda.fit,Smarket.2005)
lda.pred[1:5,]                           #Look at the first 5 observations of the predicted values
Error in lda.pred[1:5, ] : incorrect number of dimensions   #Error because data is not in matrix form
class(lda.pred)                          #What form is the dataset in? List.
[1] "list"
data.frame(lda.pred)[1:5,]               #Cast to a dataframe and then try to look at first 5 observations
     class posterior.Down posterior.Up         LD1
999     Up      0.4901792    0.5098208  0.08293096
1000    Up      0.4792185    0.5207815  0.59114102
1001    Up      0.4668185    0.5331815  1.16723063
1002    Up      0.4740011    0.5259989  0.83335022
1003    Up      0.4927877    0.5072123 -0.03792892
table(lda.pred$class,Smarket.2005$Direction)      #Create a table of the predicted classes against the true classes
       Down  Up                                   #This is a Confusion Matrix
  Down   35  35                                   #Diagonal elements are the correct classification
  Up     76 106                                   #Off-diagonal elements are the mistakes
mean(lda.pred$class==Smarket.2005$Direction)      #Classification rate of the model, which is 56%
[1] 0.5595238                                     #Model performs 6% better than chance (flipping a coin)



## K-Nearest Neighbors  (very handy to have in toolbox, effective most of the time)
library(class)  
?knn
k-nearest neighbour classification for test set from training set. For each row of the test set, the k nearest (in Euclidean distance) training set vectors are found, and the classification is decided by majority vote, with ties broken at random. If there are ties for the kth nearest vector, all candidates are included in the vote.
attach(Smarket)      #Put variable names in workspace
ls()  # Check to see it is there
Xlag=cbind(Lag1,Lag2)  #Make a matrix with lag1 and lag2
Xlag[1:5, ]   #Take a look at first five observations
train=Year<2005        # Designate train as Year less than 2005
#Make a k-Nearest Neighbor model where knn(x-variables from train, x-variables from test, class labels from train, k = ,  )
knn.pred=knn(Xlag[train,],Xlag[!train,],Direction[train],k=1)
table(knn.pred,Direction[!train])  #Generate a Confusion Matrix with predicted againsted true values
knn.pred Down Up
    Down   43 58
    Up     68 83
mean(knn.pred==Direction[!train])  #Obtain the mean, which gives model performance on prediction
[1] 0.5             #Model prediction rate is 50%, which is the same as flipping a coin
