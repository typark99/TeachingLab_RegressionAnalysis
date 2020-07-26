#############################################
# File: Lab2.R
# Author: Taeyong Park 
# Summary: Multivariate linear regression in R
#############################################

# Consider the example used in the lecture
y = c(14,20,18,8,5)
x1 = c(12,18,13,9,8)
x2 = c(5,7,10,9,8)

# OLS estimates
X = cbind(rep(1,5), x1, x2) # Create the X matrix
XX = t(X)%*%X # t(X): Transpose
# For matrix multiplications, use %*%
invXX = solve(XX) # Inverse
XY = t(X)%*%y # t(X): Transpose
b = invXX%*%XY

# variance-covariance matrix
11.255*invXX # 11.255 is from the lecture note.


# Let's compare the results to those from lm()
model = lm(y~x1+x2)
summary(model)

# variance-covariance matrix
vcov(model) 
# confidence interval
confint(model, level = .9)  



#############################################
#
# Regressiona analysis using the NBA dataset
#
#############################################

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


##################################
# Question 1: Bivariate regression
#
# Develop an estimated regression equation that 
# can be used to predict the percentage of games won 
# given the percentage field goals made. 
# Also, test for a significant relationship. 
##################################

# Let's draw a plot.
plot(nba$FG.Perc, # X-axis
     nba$Win.Perc, # Y-axis
     xlab="Field Goals Made (%)", 
     ylab="Win (%)",
     main="Scatter Plot for Field Goals and Games Won",
     pch=19,
     col="darkred")

# Run a regression analysis using the lm function.
model.nba = lm(Win.Perc ~ FG.Perc, data=nba) 
# Bivariate: lm(Outcome V ~ Explanatory V, data=the name of data)

# Use the summary() function to obtain standard errors, t-statistics, and p-values in addition to the estimates.
summary(model.nba)

# You need to report the following:
# What's the meaning of the coefficient for the explanatory variable(s)?
# Is the coefficient statistically signifcant at the 0.05 level?
# Is the coefficient statistically signifcant at the 0.01 level?
# Is the size of effect substantively large?



##################################
# Question 2: Multivariate regression
#
# Develop an estimated regression equation that can be used to 
# predict the percentage of games won 
# given the percentage of field goals made,
# the percentage of three-point shots made, 
# the average number of offensive rebounds,
# and the average number of defensive rebounds.
##################################

# If you don't remember the variable names
colnames(nba)


##########################
# Exercise 1
##########################
# Draw scatterplots for each of the independent variables against the dependent variable.
# Label X-axis, Y-axis, main title appropriately.
# Customize "pch" Google "pch in R" 
# Customize your color using
colors()


# Now run a regression analysis using the lm function.
model.nba.multi = lm(Win.Perc ~ FG.Perc + Three.Perc + RBOff + RBDef, data=nba) 
# Multivariate: lm(Outcome V ~ V1 + V2 + V3 + ... , data=the name of data)

# Use the summary() function to obtain standard errors, t-statistics, and p-values in addition to the estimates.
summary(model.nba.multi)
vcov(model.nba.multi) # variance-covariance matrix.

# You need to report the following:
# What's the meaning of the coefficient for the explanatory variable(s)?
# Is the coefficient statistically signifcant at the 0.05 level?
# Is the coefficient statistically signifcant at the 0.01 level?
# Is the size of effect substantively large?


##########################
# Exercise 2
##########################

# Consumer Research, Inc., is an independent agency that conducts research on 
# consumer attitudes and behaviors for a variety of firms.
# In one study, a client asked for an investigation of consumer chracteristics
# that can be used to predict the amount charged by credit card users.
# Data were collected on annual income, household size, and annual credit card charges
# for a sample of 50 consumers. 

##################################
# Question 
#
# Develop an estimated regression equation that predicts annual credit card charges
# with annual income and household size as the independent variables
# 
# And then, You need to report the following:
# What's the meaning of the coefficient for the explanatory variable(s)?
# Is the coefficient statistically signifcant at the 0.05 level?
# Is the coefficient statistically signifcant at the 0.01 level?
# Is the size of effect substantively large?
##################################

model.consumer.multi = lm(Amount.Charged~Household.Size+Income, data=consumer) 
