################
# File: Lab7.R
# Author: Taeyong Park 
# Summary: Model Building
#################

#####################################
# What determines employment status?
# 
# Theory: Education influences employment status.
# Control v: age, gender and race
#####################################
library(nnet) # This package makes multinom() available.

cces=read.csv("cces1.csv")
colnames(cces)

# First, you need to choose the level of your outcome that you wish to use as our baseline.
table(cces$employment)
# Among the three levels (categories), choose your baseline.
cces$employment = relevel(cces$employment, ref = "unemployed")

# Create dummy variables for education.
cces$highschool = rep(0, nrow(cces))
cces$highschool
cces$highschool[cces$education=="highschool"] = 1
cces$highschool

cces$bachelor = rep(0, nrow(cces))
cces$bachelor
cces$bachelor[cces$education=="bachelor"] = 1
cces$bachelor

cces$postgrad = rep(0, nrow(cces))
cces$postgrad
cces$postgrad[cces$education=="postgrad"] = 1
cces$postgrad

# Create dummy variables for age.
cces$above65 = rep(0, nrow(cces))
cces$above65
cces$above65[cces$age=="above65"] = 1
cces$above65

cces$below30 = rep(0, nrow(cces))
cces$below30
cces$below30[cces$age=="below30"] = 1
cces$below30

cces$middle = rep(0, nrow(cces))
cces$middle
cces$middle[cces$age=="middle"] = 1
cces$middle

# Create dummy variables for race.
cces$black = rep(0, nrow(cces))
cces$black
cces$black[cces$race=="black"] = 1
cces$black

cces$latino = rep(0, nrow(cces))
cces$latino
cces$latino[cces$race=="latino"] = 1
cces$latino

cces$white = rep(0, nrow(cces))
cces$white
cces$white[cces$race=="white"] = 1
cces$white


multinom.employ = multinom(employment ~ bachelor+postgrad+female+black+latino, data=cces)
summary(multinom.employ)


# Google form: tinyurl.com/wxewvkt

##########################################################################
# Exercise 1: What determines income?
#
# 1) Develop a theory. In other words, choose your explanatory variable.
# 2) Build your model. In other words, choose your control variables.
# 3) Run a regression analysis.
##########################################################################


#####################################
# Exercise 2: Import Job.csv and 
# take a look the data.
#
# This dataset is from Zainab Akhtar's research
# project for 36-202 in 2018 FL.
#
# For the question, what determines job attrition?,
# 
# 1) Develop a theory. In other words, choose your explanatory variable.
# 2) Build your model. In other words, choose your control variables.
# 3) Run a regression analysis.
#####################################
job = read.csv("Job.csv")
colnames(job)


# Google form: tinyurl.com/sbuuhnj

