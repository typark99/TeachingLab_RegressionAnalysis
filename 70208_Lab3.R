################################################
# File: Lab3.R
# Authors: Taeyong Park 
# Summary: Residuals, R-squared, F test
#################################################

# NBA is a professional basketball league in the US. 
# NBA records a variety of statistics for each of the 30 teams. 
# In the NBAStats.csv data for the 2011-12 season, 
# Win.Perc refers to the percentage of games won.
# FG.Perc refers to the percentage of field goals made.
# Three.Perc refers to the percentage of three-point shots made.
# FT.Perc refers to the percentage of free throws made.
# RBOff refers to the average number of offensive rebounds per game.
# RBDef refers to the average number of defensive rebounds per game.
nba <- read.csv("NBAStats.csv")
colnames(nba) 

# Run a multivariate regression analysis using the lm function.
model.nba.multi = lm(Win.Perc ~ FG.Perc + Three.Perc + FT.Perc + RBOff + RBDef, data=nba) 
# Multivariate: lm(Outcome V ~ V1 + V2 + V3 + ... , data=the name of data)

# Use the summary() function to obtain standard errors, t-statistics, and p-values in addition to the estimates.
summary(model.nba.multi)

############
# Residuals
############
# Two ways to see the residuals
# 1. The residual function
res1 = residuals(model.nba.multi)
# 2. Observations - fitted values
res2 = nba$Win.Perc- fitted(model.nba.multi) 
# Use cbind() to compare them
cbind(res1, res2)

summary(residuals(model.nba.multi))

##########################################
# Model Fit: Adjusted R-squared & F test
##########################################
summary(model.nba.multi)
# What does the value of Adjusted R-squared tell you?
# What does the values for F-statistic and its p-value tell you?

# The critical value for the F-test at 0.05
qf(0.05, df1=5, df2=24, lower.tail = F)


##########################
# Exercise
##########################

# Consumer Research, Inc., is an independent agency that conducts research on 
# consumer attitudes and behaviors for a variety of firms.
# In one study, a client asked for an investigation of consumer chracteristics
# that can be used to predict the amount charged by credit card users.
# Data were collected on annual income, household dize, and annual credit card charges
# for a sample of 50 consumers. 

##################################
# Question 
#
# Develop an estimated regression equation that 
# with annual income and household size as the independent variables
# 
# And then, You need to report the following:
# Provide a summary of residuals.
# What does the value of Adjusted R-squared tell you?
# What does the values for F-statistic and its p-value tell you?
##################################
