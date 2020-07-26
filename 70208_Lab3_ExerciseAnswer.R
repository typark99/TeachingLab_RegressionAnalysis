##########################
# Exercise
##########################

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
consumer = read.csv("Consumer.csv")
colnames(consumer)


model.consumer = lm(Amount.Charged~Income+Household.Size, data=consumer)
summary(model.consumer)

# Summary of residuals
summary(residuals(model.consumer))

# Adjusted R-sqared: The independent variables collectively explain about 82% of the variability in the outcome variable.
# F-statistic: There exists a significant relationship between the outcome variable and the set of all the independnet variables at 0.01. 