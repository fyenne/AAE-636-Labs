# AAE 636: Introduction to R
# Sunny Jie Feng
# Reference: http://faculty.marshall.usc.edu/gareth-james/ISL/code.html

##Create a new R Script

#File/New File/R Script

##Set your own directory
getwd()
##Install packages if needed

#method 1


#method 2
library("ggplot2")


#diamonds is a data in the package of ggplot2
qplot(data=diamonds, carat, price, 
      colour = clarity, facets = .~clarity)

# Basic Commands

x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
dim(x)

x+y
ls()
#rm(x,y)
#ls()
#rm(list=ls())
#--------------------------------------------
#--------------------------------------------

?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
x
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2

?rnorm #The Normal Distribution
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)

cor(x,y)  # covariation

set.seed(1303)

rnorm(50)
set.seed(3)

y=rnorm(100000)

mean(y)
var(y)
sqrt(var(y))
sd(y)
#--------------------------------------------
#--------------------------------------------


# Graphics
x=rnorm(100)
y=rnorm(100)

plot(x,y)
plot(x,y,xlab="this is the x-axis",
     ylab="this is the y-axis",main="Plot of X vs Y")
pdf("Figure.pdf")  # export .pdf
plot(x,y,col= "steelblue")

dev.off()  # delete all plots

#--------------------------------------------
#--------------------------------------------

x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=5)
y=x
outer(x,y,function(x,y)cos(y)/(1+x^2))

cos(y) %o% 1/(1+x^2)
cos(y) %o% (1/(1+x^2))
#--------------------------------------------
#--------------------------------------------

f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T) # number of levels
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)

persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data
A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)
A[A[,1]==4,]
mean(A)
sd(A)



# Loading Data
Auto=read.table("Auto.data")
View(Auto)
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
View(Auto)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
dim(Auto)

Auto[1:4,]
Auto[1:4,"mpg"]
length(Auto[1:4,])
dim(Auto[1:4,])

# Be careful about the data.frame structure
Auto[1:4,1:2]*Auto[1:4,1]#4*2 times 4*1 can work
dim(Auto[1,1:4])
class(Auto[1:4,1])
Auto[1:4,1:2]*Auto[1,1:4]#4*2 times 1*4 cannot work
dim(Auto[1,1:4])
class(Auto[1,1:4])

Auto=na.omit(Auto)
dim(Auto)
names(Auto)

# Additional Graphical and Numerical Summaries
#plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)

attach(Auto)
plot(cylinders, mpg)

cylinders=as.factor(cylinders)

plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
plot(density(mpg)) 

pairs(Auto)

pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
?pairs

plot(horsepower,mpg)

identify(horsepower,mpg,name)

summary(Auto)
summary(mpg)
?summary


