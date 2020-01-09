# AAE 636: Session 5
# Group Exercise Solutions

# ##Set your own directory
# setwd("~/AAE636/Session5")
# rm(list=ls())

##Install Wooldridge data
library("wooldridge")
library(psych)

##This package might be helpful for your hypothesis testing.
#install.packages("MASS")
#install.packages("car")
#install.packages("data.table", type = "binary") #this is quite tricky for the new version of R.
library(car)
library(data.table)

data("sleep75")
?sleep75
describe(sleep75)

#generate different sample
all <- sleep75
men <- subset(sleep75, male==1)
women <- subset(sleep75, male==0)

##Generate the summary for the sleep time(including naps)
#slpnaps: dependent variable, minutes sleep, inc. naps
#totwrk: research interest, mins worked per week
#some concern: wage; educ; health etc.

##1. Generate Table 2 in the paper: 
# what is the mean and S.D. of sleep and naps of the total; men group; women group; 
slpnaps.all <- c(mean(all$slpnaps),sd(all$slpnaps))
slpnaps.all
slpnaps.men <- c(mean(men$slpnaps),sd(men$slpnaps))
slpnaps.men
slpnaps.women <- c(mean(women$slpnaps),sd(women$slpnaps))
slpnaps.women
  
# what is the mean and S.D. of work time of the total; men group; women group;
totwrk.all <- c(mean(all$totwrk),sd(all$totwrk))
totwrk.all
totwrk.men <- c(mean(men$totwrk),sd(men$totwrk))
totwrk.men
totwrk.women <- c(mean(women$totwrk),sd(women$totwrk))
totwrk.women

# what is the relationship of sleep and naps with work time?
#method 1: correlation
cor(all$slpnaps,all$totwrk)
cor(men$slpnaps,men$totwrk)
cor(women$slpnaps,women$totwrk)

#method 2:graph
plot(slpnaps~totwrk,data=all , main="Sleep VS Work"
     ,xlab = "Work",ylab = "Sleep")

plot(slpnaps~totwrk,data=men , main="Sleep VS Work for Men"
     ,xlab = "Work",ylab = "Sleep",col = "blue")

plot(slpnaps~totwrk,data=women , main="Sleep VS Work for Women"
     ,xlab = "Work",ylab = "Sleep",col = "red")

##2. Relationship between F-stats and t-stats for one variable regression and multiple variable regressions:
# model 1. slpnaps=beta_1*totwrk+beta_0, find out the F-stats and t-stats for totwrk and prove its relationship
summary(lm(slpnaps~totwrk,data = all))

# t-stats=-9.674
# F-stats=93.6
# (-9.674)^2=93.6

# model 2. slpnaps=beta_1*totwrk+beta_2*educ+beta_0, find out the F-stats and t-stats for totwrk , is there any relationship between them?
summary(lm(slpnaps~totwrk+educ,data = all))
# t-stats=-9.629
# F-stats=52.65
# (-9.629)^2!=52.65
# We normally cannot write a close form for that.

##3. Generate Table 3
# model 3: slpnaps=beta_1*totwrk+beta_2*marr+beta_3*yrsmarr+beta_4*age+
###################+beta_5*agesq+beta_6*educ+beta_7*male+beta_8*gdhlth+
###################+beta_9*yngkid+beta_10*prot+beta_11*black+beta_0
# estimate the model for all the respondent; men; women seperately.
all.fit <- lm(slpnaps~totwrk+marr+yrsmarr+age+agesq+educ+male+gdhlth+
                yngkid+prot+black,data = all)
summary(all.fit)

men.fit <- lm(slpnaps~totwrk+marr+yrsmarr+age+agesq+educ+male+gdhlth+
                yngkid+prot+black,data = men)
summary(men.fit)

women.fit <- lm(slpnaps~totwrk+marr+yrsmarr+age+agesq+educ+male+gdhlth+
                  yngkid+prot+black,data = women)
summary(women.fit)

#Answer the question: why we would like to seperate them into subgroups? 
# Dummy variable male can capture the fixed effect of being a male.
# Seperate the groups can capture the differences in slope coefficients as well.

#Do you think all the signs of estimated coefficients reasonable? Explain.
# Always infer from the statistically significant result.

#Compare the R^2 and adjusted R^2 of the three models.
# The table 3 reported the adjusted R^2. 

##4. Hypothesis testing
# a. F-test: beta_1=beta_2=0 at 0.05 siginificance level
#method 1:
# find the critical value
k <- 11 #number of regressors(not including the intercept)
n <- dim(all)[1]
r <- 2 #number of restrictions
qf(.95,r,n-k-1)
#method 1.
linearHypothesis(all.fit,c("totwrk=0","marr=0"), test="F")
1-pf(47.743,r,n-k-1)

#method 2.
all.fit.Restrict <- lm(slpnaps~yrsmarr+age+agesq+educ+male+gdhlth+
                         yngkid+prot+black,data = all)
anova(all.fit)
anova(all.fit.Restrict)
F.stat <- (168826203-148407099)/2/(213843)
#F.stat=47.74321

#method 3: Lagrange Multiplier
all.fit.Lagrange <- lm(slpnaps~yrsmarr+age+agesq+educ+male+gdhlth+
                yngkid+prot+black,data = all)

all.fit.Lagrange.aux <- lm(all.fit.Lagrange$residuals~totwrk+marr+yrsmarr+age+agesq+educ+male+gdhlth+
                             yngkid+prot+black,data = all)
summary(all.fit.Lagrange.aux)

#Lagrange Multiplier Stat
qchisq(.95, df=2)
n*0.1209
#we reject the null hypothesis that beta_1=beta_2=0

# b. t-test: beta_1=beta_2 at 0.05 siginificance level by rewriting your model
all$totwrkm <-all$totwrk+all$marr
all.fit2 <- lm(slpnaps~totwrk+yrsmarr+age+agesq+educ+male+gdhlth+
                yngkid+prot+black+totwrkm,data = all)
summary(all.fit2)
# t-stat: -0.283
#we fail reject the null hypothesis that beta_1=beta_2

# c. t-test: beta_1=beta_2 at 0.05 siginificance level step by step BY HAND
vcov(all.fit)
t.stat <- (all.fit$coefficients['totwrk']-all.fit$coefficients['marr'])/(sqrt(0.0004166623+3.279334e+03-2*0.0541394655))
# t-stat: -0.2834963
#we fail reject the null hypothesis that beta_1=beta_2

# d. F-test: beta_1=beta_2 at 0.05 siginificance level, and what is the relationship between the F-test and t-test here?
linearHypothesis(all.fit,c("totwrk=marr"), test="F")
#F-stat:0.0804=(-0.2834963)^2
#we fail reject the null hypothesis that beta_1=beta_2

