# AAE 636: Session 3
# Group Exercise

# ##Set your own directory
# getwd()
# setwd("~/AAE636/Session3")
# rm(list=ls())
# 
# ##Load the Rdata
# load("~/AAE636/Assignment 1/wage2.RData")

library(tidyverse)
library(ggplot2)
library(tidyverse)
library(grid)
library(psych)
library(wooldridge)
library(BSDA)
#lets keep only the wages and education and delete all the NAs

attach(wage2)

#1. What are the sample means and standard deviations of wages and of education?
ybar <- mean(wage)
ysd <- sd(wage)#sample sd
yvar <- var(wage)

xbar <- mean(educ)
xsd <- sd(educ)#sample sd
xvar <- var(educ)

#2. Plot wages against education(education as x and wages as y). Is the correlation
#between wages and education positive or negative? Does the correlation appear to
#be strong or weak? Explain.
plot(educ,wage,  main="Higher Education, Higher Wages?"
     ,xlab = "Education",ylab = "Wages")
boxplot(wage~educ,data=sample, main="Higher Education, Higher Wages?",
        xlab = "Education",ylab = "Wages")

#3. What is the covariance between wages and education? What is the correlation be-
#  tween wages and education?
xycov <- cov(educ, wage)
xycov
xycor <-cor(educ, wage)
xycor

#  4. We are interested in estimating the regression model
#################Wage =beta0 + beta1 Educ + u

## Recall from last time:
## lm(formula = wage ~ educ, data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -877.38 -268.63  -38.38  207.05 2148.26 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  146.952     77.715   1.891   0.0589 .  
## educ          60.214      5.695  10.573   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 382.3 on 933 degrees of freedom
## Multiple R-squared:  0.107,  Adjusted R-squared:  0.106 
## F-statistic: 111.8 on 1 and 933 DF,  p-value: < 2.2e-16

#Calculate BY HAND(Step by step) the OLS regression coefficients for the intercept
#and slope terms in a regression of wage on education.
beta1 <- sum((educ-xbar)*(wage-ybar))/sum((educ-xbar)^2)
beta1
#or
beta1 <- xycov/xvar
beta1

beta0 <- ybar-beta1*xbar

#5. What does the estimated coefficient, ^{beta1}, tell you about the relationship between
#wages and education? Is it causal? how can you explain the coefficient?
#With one additional year of education, the expected wage is going to increase by $60.214. 
#But we cannot tell whether this causal or not, more justification needed for our conclusion.


#6. Calculate the residuals, and plot the residuals against education, what can you learn
#from the graph.
attach(wage2)
yhat <- beta1*educ+beta0

residuals <- wage-yhat
plot(educ,residuals, xlab = "Education",ylab = "Residuals")
boxplot(residuals~educ,data=sample, main="Residual Graph",
        xlab = "Education",ylab = "Residuals")

# The assumption of homoskedasiticity might be violated. We will learn later on how to deal with this.

#7. Calculate the SST, SSE and SSR, and which one or none of these is the OLS estimator
#for sigma^2?

#total sum of squares (SST)
SST <- sum((wage-ybar)^2)

#residual sum of squares (SSR)
SSR <- sum((wage-yhat)^2)

#explained sum of squares (SSE)
SSE <-sum((yhat-ybar)^2)

isTRUE(SST==SSE+SSR)

#However, none of them is the OLS estimator of sigma^2, 
#the OLS estimator of sigma^2 is: SSR/(n-k-1)
#Here we have k=1
n <- dim(sample)[1]
OLSsigma <- SSR/(n-1-1)
OLSsigma

#  8. BY HAND(Step by step), compute the estimates of the variances of ^{beta0} and ^{beta1} by
#inserting the estimate ^{sigma^2} into the appropriate formulas.
var.beta0 <- OLSsigma * sum(educ^2)/(n*(n-1)*xvar)
#or 
var.beta0 <- OLSsigma * sum(educ^2)/(n*sum((educ-xbar)^2))

var.beta1 <- OLSsigma/((n-1)*xvar)
#or
var.beta1 <- OLSsigma/(sum((educ-xbar)^2))

se.beta0 <- sqrt(var.beta0)
se.beta1 <- sqrt(var.beta1)

#9. BY HAND(Step by step)(Step by step), create a 95% conffidence interval for the
#slope parameter ^{beta1}.

#Generate the Z-statistics or t-statistics for the confidence interval, I am using t-stats
#since it is more accurate.
dof <- n-1-1
t <- qt(1-.05/2,dof)

#generate the 95% CI
CI <- c(beta1-t*se.beta1,beta1+t*se.beta1)

#10. Calculate t-statistics, p-value for education and decide whether we accept the Null
#hypothesis at the signifficance level of 95%: ^{beta1} is zero.

t.stat <- (beta1-0)/se.beta1
isTRUE(abs(t.stat)>t)

p <- 2*pt(-abs(t.stat),dof)
p
isTRUE(p<.05)

#We reject the null hypothesis at the siginificance level of 95% that ^{beta1} is zero.

#11. Calculate F-statistics for the regression equation.

F.stat <- (SSE/1)/(SSR/dof)#always devide by the corresponding degree of freedom.
F.stat
#pvalue of F statistics, not required by the class I guess
1-pf(F.stat,1,dof)

#We say that the educ is (jointly) siginificant at the 95% siginificance level.

#12. Calculate the R2 and the adjusted R2, and explain the value.

r2 <- SSE/SST
#or 
r2 <- 1-SSR/SST
r2
#the proportion of the total variation in the yi’s that is explained by the regression model; 
#goodness of fit

adjusted.r2 <- 1-(SSR/dof)/(SST/(n-1))
adjusted.r2

#we have to adjust to the different dof for different variance.
