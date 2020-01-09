# AAE 636: Session 5
# Group Exercise

##Set your own directory
# setwd("~/AAE636/Session5")
# rm(list=ls())

##Install Wooldridge data
library("wooldridge")
library(psych)

##This package might be helpful for your hypothesis testing.
#install.packages("MASS")
#install.packages("car")
#install.packages("data.table", type = "binary") #this is quite tricky for the new version of R.
#library(car)
#library(data.table)

data("sleep75")

?sleep75
describe(sleep75)
attach(sleep75)

##Generate the summary for the sleep time(including naps)
#slpnaps: dependent variable, minutes sleep, inc. naps
#totwrk: research interest, mins worked per week
#some concern: wage; educ; health etc.

##1. Generate Table 2 in the paper: 
# what is the mean and S.D. of sleep and naps of the total; men group; women group; 
# what is the mean and S.D. of work time of the total; men group; women group;
# what is the relationship of sleep and naps with work time?

##2. Relationship between F-stats and t-stats for one variable regression and multiple variable regressions:
# model 1. slpnaps=beta_1*totwrk+beta_0, 
# find out the F-stats and t-stats for totwrk and prove its relationship
# model 2. slpnaps=beta_1*totwrk+beta_2*educ+beta_0, find out the F-stats and t-stats for totwrk , is there any relationship between them?

##3. Generate Table 3
# model 3: slpnaps=beta_1*totwrk+beta_2*marr+beta_3*yrsmarr+beta_4*age+
###################+beta_5*agesq+beta_6*educ+beta_7*male+beta_8*gdhlth+
###################+beta_9*yngkid+beta_10*prot+beta_11*black+beta_0
# estimate the model for all the respondent; men; women seperately.

#Answer the question: why we would like to seperate them into subgroups? 
##################### do you think all the signs of estimated coefficients reasonable? Explain.
##################### compare the R^2 and adjusted R^2 of the three models.

##4. Hypothesis testing
# a. F-test: beta_1=beta_2=0 at 0.05 siginificance level
# b. t-test: beta_1=beta_2 at 0.05 siginificance level by rewriting your model
# c. t-test: beta_1=beta_2 at 0.05 siginificance level step by step BY HAND
# d. F-test: beta_1=beta_2 at 0.05 siginificance level, and what is the relationship between the F-test and t-test here?


