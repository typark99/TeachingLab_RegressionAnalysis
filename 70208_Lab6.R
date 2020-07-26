################
# File: Lab6.R
# Author: Taeyong Park 
# Summary: Multinomial Logistic regression
#################
# install.packages("nnet"): run to install the nnet package. 
# Once installed, you don't have to run this install function again.

library(nnet) # Once installed, you can load the package using the library function.
# This package makes multinom() available.

cces=read.csv("cces1.csv")
colnames(cces)

# Suppose you want to examine whether the probability of voting 
# in the US 2016 presidential election is influenced 
# by income and employment status.

# First, you need to choose the level of your outcome that you wish to use as your baseline.
table(cces$pvote3)
# Among the three levels (categories), choose your baseline.
cces$pvote3 = relevel(cces$pvote3, ref = "Johnson")

# Look into the income and employment variables.
class(cces$income)
table(cces$income) 

class(cces$employment)
table(cces$employment)

# Create dummy variables for employment status, and set fulltime as the baseline.
cces$unemployed = rep(0, nrow(cces))
cces$unemployed
cces$unemployed[cces$employment=="unemployed"] = 1
cces$unemployed

cces$parttime = rep(0, nrow(cces))
cces$parttime
cces$parttime[cces$employment=="parttime"] = 1
cces$parttime

cces$fulltime = rep(0, nrow(cces))
cces$fulltime
cces$fulltime[cces$employment=="fulltime"] = 1
cces$fulltime

# Run a multinomial logistic model
vote.1 = multinom(pvote3~income+unemployed+parttime, data=cces)
summary(vote.1)

# Relative risk ratios
# The relative risk ratio for moving from a full time worker to 
# an unemployed person for voting for Trump rather than Johnson
exp(0.09915202)

# The relative risk ratio for one-unit increase in income
# for voting for Clinton rather than Johnson
exp(0.04108836)

# Predictions
newdata.1 = data.frame(income = mean(cces$income), unemployed = 1, parttime=0)
y.pred.1 = predict(vote.1, newdata = newdata.1, type="probs") # props type is for the multinom function

newdata.2 = data.frame(income = mean(cces$income), unemployed = 0, parttime=0)
y.pred.2 = predict(vote.1, newdata = newdata.2, type="probs") # props type is for the multinom function

y.pred.2 - y.pred.1

newdata.3 = data.frame(income = 5, unemployed = 0, parttime=0)
y.pred.3 = predict(vote.1, newdata = newdata.3, type="probs") # props type is for the multinom function

newdata.4 = data.frame(income = 5, unemployed = 0, parttime=1)
y.pred.4 = predict(vote.1, newdata = newdata.4, type="probs") # props type is for the multinom function

y.pred.4 - y.pred.3


# Test statistics and p-value
estimate = summary(vote.1)$coefficients
std.error = summary(vote.1)$standard.errors
z.stat = estimate/std.error

# p-value for the effect of income on voting for Clinton
# rather than Johnson
2*pnorm(z.stat[1,2], lower.tail = F) 

# p-value for the effect of moving from full-time to unemployed
# on voting for Trump rather than Johnson
2*pnorm(z.stat[2,3], lower.tail = F)

# Confidence intervals
confint(vote.1)
confint(vote.1, level = .99)


################################################
# Exercise 1
# 1. Set Clinton as the baseline, and run the analysis again.
# 2. The relative risk ratio for moving from a full time worker to 
# an unemployed person for voting for Trump rather than Clinton?
# 3. The relative risk ratio for one-unit increase in income
# for voting for Trump rather than Clinton?
# 4. Predicted probabilities for voting for the three candiates for a voter 
# whose income level is 3 and employment status is unemployed?
# 5. What's the effect of moving from the voter whose income level is 3
# and employment status is unemployed to another voter whose income level
# is 3 and employment status is fulltime?
# 6. test statistic for the impact of moving from full time to part time on the prob of
# voting for Johnson rather than Clinton?
# 7. p-value for the impact of income on the prob of
# voting for Trump rather than Clinton?
################################################


################################################
# Exercise 2
# Suppose you realize that it's important to control for gender and party affiliation
# to estimate the impacts of income and employment status accurately.
# Run a new model to take into account your new control variables.
# Set Clinton as the baseline category for the outcome variable. 
# Set Democrat as the baseline category for the party variable.
# Set fulltime as the baseline category for employment status.
################################################


################################################
# Exercise 3
# Suppose your research hypothesis is that marital status is affected by
# people's perception about their work-life balance. 
# To estimate the relationship you will control for 
# age, gender, and job satisfaction. 
#
# Import Job.csv data. Run a regression model to test your hypothesis
# using the following variables:
# MaritalStatus: Single/Married/Divorced (Set Single as the baseline.)
# WorkLifeBalance: Numerical values; The higher, the more balanced (treat this as continuous)
# Age: Numerical values
# Gender: Female/Male (Create a dummy variable such that Female = 1; Male = 0)
# JobSatisfaction: Numerical values; The higher, the more satisfied (treat this as continuous)
################################################


################################################
# Exercise 4
# Based on the model from Exercise 3, answer the following questions.
#
# 1. What's the effect of a one level increase in the perception of work-life balance
# on the log odds of being Married rather than Single?
# 2. What's the effect of a one level increase in job satisfaction
# on the log odds of being Divorced rather than Single?
# 3. The relative risk ratio for moving from a man to 
# a woman for being Married rather than Single?
# 4. The relative risk ratio for a one level increase in the perception of work-life balance
# for being Married rather than Single? 
# 5. Predicted probability for being Married for a male 
# whose age is 40 and job satisfaction level is 3?
# 6. test statistic for the impact of job satisfaction on the prob of
# being Divorced rather than Single?
# 7. p-value for the impact of the perception of work-life balance on the prob of
# being Married rather than Single?
# 8. Build a 95% Confidence interval for the effect of a one level increase in the perception of work-life balance
# on the log odds of being Married rather than Single.
################################################

