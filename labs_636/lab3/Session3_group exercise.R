# AAE 636: Session 3
# Group Exercise
library(tidyverse)
library(ggplot2)
library(tidyverse)
library(grid)
library(psych)
library(wooldridge)
library(BSDA)

##Set your own directory
getwd()
# setwd("~/AAE636/Session3")
rm(list=ls())
describe(wage2)
data <-  wage2
##Load the Rdata
# load("~/AAE636/Assignment 1/wage2.RData")

##Take a look at your data first
#method 1
# install.packages("pastecs")
# library(pastecs)
# stat.desc(data)

#method 2
# install.packages("psych")
# library(psych)
describe(data)

#lets keep only the wages and education and delete all the NAs
sample <- data.frame(data['wage'], data['educ'])
dim(sample)
sample <- na.omit(sample)#seems like we don't have any missing values
dim(sample)

attach(sample)

#1. What are the sample means and standard deviations of wages and of education?
ybar <- mean(wage)
yvar <- sd(wage)#sample sd
xbar <- mean(educ)
xvar <- sd(educ)#sample sd

#2. Plot wages against education(education as x and wages as y). Is the correlation
#between wages and education positive or negative? Does the correlation appear to
#be strong or weak? Explain.
plot(educ,wage, xlab = "Education",ylab = "Wages")
boxplot(wage~educ)
lm.fit <- lm(formula = wage~educ,data = data)
summary(lm.fit)
#3. What is the covariance between wages and education? What is the correlation be-
#  tween wages and education?
cov(educ, wage)
cor(educ, wage)

#  4. We are interested in estimating the regression model
#################Wage =beta0 + beta1 Educ + u
#Calculate BY HAND(Step by step) the OLS regression coefficients for the intercept
#and slope terms in a regression of wage on education.


#5. What does the estimated coefficient, ^{beta1}, tell you about the relationship between
#wages and education? Is it causal? how can you explain the coefficient?


#6. Calculate the residuals, and plot the residuals against education, what can you learn
#from the graph.


#7. Calculate the SST, SSE and SSR, and which one or none of these is the OLS estimator
#for sigma^2?
ssr = sum(residual^2)
sse = sum((yhat - ybar)^2)
sse = sum((wage - ybar)^2)

#  8. BY HAND(Step by step), compute the estimates of the variances of ^{beta0} and ^{beta1} by
#inserting the estimate ^{sigma^2} into the appropriate formulas.

beta <- 
se.beta1 <- 


#9. BY HAND(Step by step)(Step by step), create a 95% conffidence interval for the
#slope parameter ^{beta1}.
qnorm(1-0.05/2)
lower95 <- beta1 - (qnorm(1-0.05/2)*se.beta1)
upper95 <- beta1 + (qnorm(1-0.05/2)*se.beta1)

#10. Calculate t-statistics, p-value for education and decide whether we accept the Null
#hypothesis at the signifficance level of 95%: ^{beta1} is zero.

#11. Calculate F-statistics for the regression equation.
# f <- qnorm(1-.05/2)
ybar <- mean(wage)
yvar <- sd(wage)#sample sd
xbar <- mean(educ)
xvar <- sd(educ)
fit<-aov(educ~wage,data=data)
fit
summary(fit)

# mwage <- mean(data$wage)
# sdmwage <- sd(A5$wage)
# lenm<-length(A5$wage)
# z.test(A5$wage, alternative = "two.sided", mu = 1100, sigma.x = sdmwage, conf.level = 0.95)
# z.estimate <- (mwage - 1100)/sdmwage*lenm^0.5
# z.estimate
# isTRUE(z.estimate>z)
f.stat <- (sse/1)(ssr/dof)
1-fp(F.stat,1,dof)

#12. Calculate the R2 and the adjusted R2, and explain the value.
