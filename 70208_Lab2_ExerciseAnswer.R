##########################
# Exercise 1
##########################
# Draw scatterplots for each of the independent variables against the dependent variable.
nba <- read.csv("NBAStats.csv")

# Three.Perc
plot(nba$Three.Perc, # X-axis
     nba$Win.Perc, # Y-axis
     xlab="Three Point Goals Made (%)", 
     ylab="Win (%)",
     main="Scatter Plot for \n Three Point Goals and Games Won",
     pch=14,
     col="salmon")

# RBOff
plot(nba$RBOff, # X-axis
     nba$Win.Perc, # Y-axis
     xlab="Avg Number of Offensive Rebounds", 
     ylab="Win (%)",
     main="Scatter Plot for \n Offensive Rebounds and Games Won",
     pch=17,
     col="violetred")

# RBDef
plot(nba$RBDef, # X-axis
     nba$Win.Perc, # Y-axis
     xlab="Avg Number of Defensive Rebounds", 
     ylab="Win (%)",
     main="Scatter Plot for \n Defensive Rebounds and Games Won",
     pch=15,
     col="steelblue")

##########################
# Exercise 2
##########################

# Consumer Research, Inc., is an independent agency that conducts research on 
# consumer attitudes and behaviors for a variety of firms.
# In one study, a client asked for an investigation of consumer chracteristics
# that can be used to predict the amount charged by credit card users.
# Data were collected on annual income, household dize, and annual credit card charges
# for a sample of 50 consumers. 

consumer = read.csv("Consumer.csv")

##################################
# Question 
#
# Develop an estimated regression equation that 
# with annual income and household size as the independent variables
# 
# And then, You need to report the following:
# What's the meaning of the coefficient for the explanatory variable(s)?
# Is the coefficient statistically signifcant at the 0.05 level?
# Is the coefficient statistically signifcant at the 0.01 level?
# Is the size of effect substantively large?
##################################
model.consumer = lm(Amount.Charged~Income+Household.Size, data=consumer)
summary(model.consumer)


# Answers:

# What's the meaning of the coefficient for the explanatory variable(s)?
# A 1,000 dollar increase in annual income will increase the amount charged by credit card users by about 33 dollars, controlling for all the other independent variables.
# A one member increase in household size will increase the amount charged by credit card users by about 356 dollars, controlling for all the other independent variables.

# Is the coefficient statistically signifcant at the 0.05 level?
# Yes for both the independent variables.

# Is the coefficient statistically signifcant at the 0.01 level?
# Yes for both the independent variables.

# Is the size of effect substantively large?
# It seems to me that the size of effects are substantively large.
# There is no "the correct answer" to this type of questions. But this is often the question that you are asked to provide in practice.


