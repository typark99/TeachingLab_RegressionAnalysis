######################################
# Exercise - Percent under 21 and fatal accidents
######################################

## Research Question: Are young drivers more likely to have fatal accidents?
## Import the Safety.csv data from 42 cities and answer the following questions.

# 1. Draw a bivariate plot to summarize the data. Place your explanatory and outcome variables properly.

# 2. Run a linear regression model and answer the following:

safety = read.csv("Safety.csv")

##### 1. Develop numerical and graphical summaries of the data.

# Numerical summary
summary(safety)

# Graphical summary
# To draw a bivariate plot, think about what variable should be the outcome V.
plot(safety$Percent.Under.21, 
     safety$Fatal.Accidents.per.1000, 
     xlab="Under 21 (%)", 
     ylab="Fatal Accidents (1000s)")

# Would you like to make the plot clearer?
plot(safety$Percent.Under.21, 
     safety$Fatal.Accidents.per.1000, 
     xlab="Under 21 (%)", 
     ylab="Fatal Accidents (1000s)",
     pch=19,
     col="violetred")

##### What do you learn from the summaries?


##### 2. Use regression analysis to investigate the relationship between the two variables.

model.safety = lm(Fatal.Accidents.per.1000 ~ Percent.Under.21, data=safety)
model.safety

plot(safety$Percent.Under.21, 
     safety$Fatal.Accidents.per.1000, 
     xlab="Under 21 (%)", 
     ylab="Fatal Accidents (1000s)",
     pch=19,
     col="violetred")
abline(model.safety, col="navyblue")

##### 3. Significance Test 

summary(model.safety)


# a) What's the meaning of the coefficient for the explanatory variable?
# A 1 percentage point increase in the proportion of population under 21 will increase the amount of fatal accidents per 1000 people by 0.28. 

# b) Is the coefficient statistically signifcant at the 0.05 level?
# Yes

# c) Is the coefficient statistically signifcant at the 0.01 level?
# Yes

# d) Is the size of effect substantively large?
# Depends on your judgment.
# In my opinion, it's large as the results indiate that a 1 percentage point increase in the proportion of population under 21 will increase the amount of fatal accidents per 100,000 people by 28. 

