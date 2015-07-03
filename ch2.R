### Week1  Chapter 2  Statistical Learning using R 
### vectors, data, matrices, subsetting
# Note: there are no scalars in R

setwd("/Users/karenyang/Desktop")

install.packages("ISLR")  #Auto data located in ISLR package
library(ISLR)  #  Load the library ISLR



x=c(2,7,5)   #Assign 3 numbers to vector 3
x
[1] 2 7 5

y=seq(from=4,length=3,by=3)  #Another way to make a vector, using a sequence
?seq    #Opens up R help on sequence

y
[1]  4  7 10

x+y    #Sum of 2 vectors of the same length, element by element
[1]  6 14 15

x/y    #Division of 2 vectors, element-wise
[1] 0.5 1.0 0.5

x^y    #x to the power of y, element-wise
[1]      16  823543 9765625

x[2]   #Use square brace to subset: extract 2nd element of x
[1] 7  

x[2:3]  #Use colon to indicate start at element 2 and stop at element 3 of subsetted vector, return subset
[1] 7 5

x[-2]   #Remove element 2 from x and return the subsetted vector as output
[1] 2 5

x[-c(1,2)]   #Remove a collection of indices 1 and 2 and return the subsetted vector
[1] 5


z=matrix(seq(1,12),4,3)   #A 2-way array is a matrix, make a 4x3 matrix and fill values 1-12 column-wise
z
     [,1] [,2] [,3]
[1,]    1    5    9
[2,]    2    6   10
[3,]    3    7   11
[4,]    4    8   12


z[3:4,2:3]            #Subset elements of matrix, 3-4 rows and 2-3 columns
     [,1] [,2]
[1,]    7   11
[2,]    8   12


z[,2:3]              #Subset columns 2 and 3 of z and return subsetted matrix
     [,1] [,2]
[1,]    5    9
[2,]    6   10
[3,]    7   11
[4,]    8   12

z[,1]                #Subset the 1st column of z. Note that it returns a vector and drops its matrix status.         
[1] 1 2 3 4

z[,1,drop=FALSE]     #Drop=FALSE keeps it as a 1-column matrix
     [,1]
[1,]    1
[2,]    2
[3,]    3
[4,]    4


dim(z)             #Dimensions of matrix is 4 rows and 3 columns
[1] 4 3


ls()            #Tells you what is available in your working directory
[1] "Auto" "x"    "y"    "z"  

rm(y)            #Remove y

ls()             #Verify that y is gone
[1] "Auto" "x"    "z"  


### Generating random data, graphics
x=runif(50)   #Random uniform from 0 to 1
y=rnorm(50)   #Random normal from 0 to 1
plot(x,y)     #plots x and y
plot(x,y,xlab="Random Uniform",ylab="Random Normal",pch="*",col="blue")
par(mfrow=c(2,1))   #Make a panel of plots with 2 rows and 1 column
plot(x,y)
hist(y)              #Make a histogram of y
par(mfrow=c(1,1))    #Reset the mfrow command


### Reading in data
#Auto=read.csv("Auto.csv")   #Already have data via ISLR package so just call data
data(Auto)

names(Auto)           #Gives names of variables
[1] "mpg"          "cylinders"    "displacement" "horsepower"   "weight"      
[6] "acceleration" "year"         "origin"       "name" 


dim(Auto)
[1] 392   9

class(Auto)        #What type of object
[1] "data.frame"


str(Auto)          #Structure of data
'data.frame':	392 obs. of  9 variables:
 $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
 $ cylinders   : num  8 8 8 8 8 8 8 8 8 8 ...
 $ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
 $ horsepower  : num  130 165 150 150 140 198 220 215 225 190 ...
 $ weight      : num  3504 3693 3436 3433 3449 ...
 $ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
 $ year        : num  70 70 70 70 70 70 70 70 70 70 ...
 $ origin      : num  1 1 1 1 1 1 1 1 1 1 ...
 $ name        : Factor w/ 304 levels "amc ambassador brougham",..: 49 36 231 14 161 141 54 223 241 2 ...ls()

summary(Auto)    #Gives summary of data
      mpg          cylinders      displacement     horsepower        weight    
 Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0   Min.   :1613  
 1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0   1st Qu.:2225  
 Median :22.75   Median :4.000   Median :151.0   Median : 93.5   Median :2804  
 Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5   Mean   :2978  
 3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0   3rd Qu.:3615  
 Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0   Max.   :5140  
                                                                               
  acceleration        year           origin                      name    
 Min.   : 8.00   Min.   :70.00   Min.   :1.000   amc matador       :  5  
 1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000   ford pinto        :  5  
 Median :15.50   Median :76.00   Median :1.000   toyota corolla    :  5  
 Mean   :15.54   Mean   :75.98   Mean   :1.577   amc gremlin       :  4  
 3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000   amc hornet        :  4  
 Max.   :24.80   Max.   :82.00   Max.   :3.000   chevrolet chevette:  4  
                                                 (Other)           :365 
                                                 
                                                 
plot(Auto$cylinders,Auto$mpg)  
plot(Auto$cyl,Auto$mpg)

attach(Auto)       #Attach dataframe, creates a workspace
search()        #You can see "Auto" available for direct use so you don't need to use $cylinders or $mpg
 [1] ".GlobalEnv"        "Auto"              "package:ISLR"      "tools:RGUI"       
 [5] "package:stats"     "package:graphics"  "package:grDevices" "package:utils"    
 [9] "package:datasets"  "package:methods"   "Autoloads"         "package:base" 
  
plot(cylinders,mpg)   #Use variables directly
cylinders=as.factor(cylinders)  #Cast as factor
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
pdf(file="../mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
quartz 
     2 
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)

#TO SUBSET DATA TO OBTAIN ROWS THAT FALL OUTSIDE OF ROWS 10-85 
Auto2 = Auto[-10:-85, ]


#TO PARTITION DATASET INTO TRAIN AND TEST SETS
# load the library that contains the data set
library(ISLR)
# readme for data set
?Auto
# readme for sample() function
?sample

# create a vector of row indexes
training <- sample(nrow(Auto), size=200)

# create training data set with 200 obs
train_set <- Auto[training,]

# create test data set with the remaing obs
test_set <- Auto[-training,]

q()