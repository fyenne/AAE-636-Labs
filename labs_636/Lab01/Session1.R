# AAE 636: Random Variables
# Sunny Jie Feng
# Functions, Loops & Random Variables
# Reference: http://pages.stat.wisc.edu/~jgillett/327-1/
###########: https://www.math.csi.cuny.edu/Statistics/R/simpleR/stat007.html
###########: https://www.statmethods.net/stats/ttest.html


## 0. Set your own directory
getwd()
setwd("~/AAE636/Session1")

##Load the Rdata
load("/Users/SunnyJieFeng/AAE636/Assignment 1/wage2.RData")
summary(data)

##################################################################
##1. Write your own function
#Define your function first
baby.max = function(a, b) { #input: a & b
  if (a > b) {
    return(a) #output 1
  } else {
    return(b) #output 2
  }
}

#test the output of our baby.max(), to be commented out after the code is finalized
stopifnot(isTRUE(all.equal(4, baby.max(3,  4))))
stopifnot(isTRUE(all.equal(3, baby.max(3, -4))))

# Second example
square.a = function(a = 1, b = 2) {#input with default values
  #printing the value of parameters/local variables by cat() is good for debugging!
  cat(sep = "", " square.a(a=", a, ", b=", b, ")\n") 
  b = 100 #local variable
  c = a*a #local variable
  return(c)
}

square.a(a = 3, b = 4)

#How to debug the code inside a function?#
#1. Comment out the beginning and the ending line
#2. Replace all the return(value) with value.
#3. Assign some testing values to the parameters.

##################################################################
##2. Loops
#for loop
for (file.name in head(list.files())) { # sequence is the first 6 file names (because we use head()) 
  # in your working directory
  # list.files() gives all file names
  # in your working directory
  cat("file.name =", file.name, "\n") #always use cat() to print
}

#while loop
#while()
#while (CONDITION) { #When the condition is TRUE, run the expression repeatedly.
#  EXPRESSION        #Zero (since we test the condition at the beginning) or more times or never quit!
#}
x <- 1
while (x < 10) {
  cat("x =", x, "\n")
  x = 2*x
}

#repeat loop
repeat {
  cat("Do you like Econometrics? Please answer 'Yes' or 'No':")
  decision <- scan(what=character(), n=1, quiet=TRUE) # ?scan. Actually, get your input on the console.
  if (decision == "Yes") { #You have to set up a way to quit. Otherwise, a deadlock!
    break #A must!
  }
}

#Make your own loop more efficient
#1. Always test the sequences in for() for debugging.
#2. Always print some variable by cat() inside the loop for debugging.
#3. Focus on one of loop commands first, like for() or while().

##################################################################
##3. Random Variables

#3.1 Draw the random variable with R
#Discrete Examples
sample(1:6,10,replace=TRUE)

data <- rnorm(100)
sample(data,10,replace=TRUE) #We will come back to this command when(if) we learn Bootstrap

#Bernoulli
rbinom(10, size=1,prob=.5)# Single trial simulation
rbinom(7, size=150,prob=.05)# 7 times of size=150 trials

?rbinom

#Uniform
runif(1,0,2)

#Normal
rnorm(10)    #Normal distribution
?rnorm

# joint distribution
#joint normal random draw
#install.packages("mvtnorm")
require("mvtnorm")
sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
head(x)
#how to do a more efficient draw: Halton draw if you are going to deal with the simulated method of moments
#install.packages("randtoolbox")
require("randtoolbox")     
halton(10, dim = 2)  #the simulated method of moments

## 3.2 CDF and PDF calculation
#Self-defined function
acumulated.distrib= function(sample,x){          #cdf
  minors= 0
  for(n in sample){
    if(n<=x){
      minors= minors+1
    }
  }
  return (minors/length(sample))
}
#--------------------------------------------
#--------------------------------------------

set.seed(1)
mysample = rnorm(100)
acumulated.distrib(mysample,1.21)

#Predefined r function
cdfeasy <- ecdf(X)    # P is a function giving the empirical CDF of X
cdfeasy(1.21)         # This returns the empirical CDF at 1.21 
plot(cdfeasy)   
#?ecdf
##Density Calculation
#For the discrete cases
x <- sample(letters[1:4],1000,replace=TRUE)
prop.table(table(x))
library(ggplot2)
ggplot(x, aes(y = density(x)))+geom_point()
#For the continous cases:we always use kernel density for illustration purpose
#Why we want to use kernel density instead of real empirical density?
plot(density(mysample))
dens<-density(mysample)
dens$y #calculated density values
head(dens$y)
#How do we calculate the empirical expectation?
#for the sample mean and sd
mean(mysample)
sum(dens$x*dens$y/length(mysample))#it is smoothed so not the same.
var(mysample)
sd(mysample)

# for the population sd
sd.p=function(x){sd(x)*sqrt((length(x)-1)/length(x))}
sd.p(mysample)
#--------------------------------------------
#--------------------------------------------

#How to calculate the joint distribution
#example from the lecture
Y <- c(0,1)
lenY <- length(Y)
X <- seq(10,14)
lenX <- length(X)
#joint pmf:fX,Y(x,y)
pmf <- matrix(c(0.08,0.02,
                0.035,0.015,
                0.375,0.375,   ##@
                0.024,0.056,
                0.002,0.018),ncol=2, byrow = T)
pmf
#fX(x) 
fx <- rowSums(pmf)
fx

#fY(y)
fy <- colSums(pmf)
fy
#conditional@ distribution
#fY|X(y|x)=fX,Y(x,y)/fX(x)
fyx <- pmf/fx   #fY|X(y|x)
fyx

#expected@ value #of Y conditional on each X
eYx <- rowSums(Y*fyx)
eYx
#covariance@
eX <- sum(X*fx)
eY <- sum(Y*fy)
eXY <- sum((X %o% Y)*pmf)
covariance <- eXY-eX*eY
covariance

#correlation@
varX <-sum(fx*((X-eX)^2))
varY <-sum(fy*((Y-eY)^2))
correlation <- covariance/sqrt(varX*varY)
correlation
#--------------------------------------------
#--------------------------------------------

##t-test
# independent 2-group t-test
y<-rnorm(100)
x<-sample( c('T','F'),100,replace = T)
t.test(y~x) # where y is numeric and x is a binary factor

y1 <- rnorm(100)
y2 <- rnorm(100,1,1)

#a quick way to check if you are doing the right thing in the assignment
mean(y1)
mean(y2)
var(y1)
var(y2)
cov(y1,y2)
cor(y1,y2)

# independent 2-group t -test
t.test(y1,y2) # where y1 and y2 are numeric

# paired t-test
t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric

# one sample t-test
t.test(y,mu=0) # Ho: mu=0

y<-rnorm(100000)
t.test(y,mu=0) # Ho: mu=0

y<-rnorm(100000000)
t.test(y,mu=0) # Ho: mu=0

dat <- seq(100,10000,length=1000)
len <- length(dat)
tscore <- rep(0,times=len)

for (i in 1:len){
  y = rnorm(dat[i])
  tscore[i] = t.test(y,mu=0)$statistic
}
plot(dat[1:len], tscore)

for (i in 1:len){
  set.seed(1) #increase the sample from previous sample base
  y = rnorm(dat[i])
  tscore[i] = t.test(y,mu=0)$statistic
}

plot(dat[1:len], tscore)

# So, is t-statistics decreasing with the increase of sample size?
