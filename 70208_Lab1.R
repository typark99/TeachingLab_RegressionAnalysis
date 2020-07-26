################
# File: Lab1.R
# Authors: Taeyong Park 
# Summary: Bivariate linear regression in R
#################

# Consider the data from the lecture:
y=c(58,105,88,118,117,137,157,169,149,202)
x=c(2,6,8,8,12,16,20,20,22,26)

# Run a regression analysis
model = lm(y~x) # lm(Dependent ~ Independent)
# Output
summary(model)


# Consider the data from the exercise question in the lecture:
y=c(3.8,3,3.5,2.8,2.4,2.7)
x=c(3.5,3.3,4,2.3,1.8,2.7)

# Run a regression analysis
model = lm(y~x)
# Output
summary(model)


########################################
# IMPORTING A DATASET 
########################################

# Import the pizza data.
pizza = read.csv("pizza.csv")
colnames(pizza)

########################################
# DRAWING A SCATTERPLOT 
########################################
plot(pizza$Population,  # X-axis (Explanatory; Independent)
     pizza$Sales) # Y-axis (Outcome; Dependent)

# Let's make the plot similar to the plot we saw in the lecture slides.
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     pch=20,
     col="darkred")

# We can also manipulate the range of X-axis and Y-axis.
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     xlim= c(0,26), ylim=c(0,220),
     pch=20,
     col="darkred")

# The plot in the slides doesn't have top and right axes. Okay, remove them!
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     xlim= c(0,26), ylim=c(0,220),
     pch=20,
     col="darkred",
     axes=F)
axis(1) # The bottom axis
axis(2) # The left axis

# Do you want to have more ticks on the X and Y axes just like the plot in the slides?
# Of course, you can do!
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     xlim= c(0,26), ylim=c(0,220),
     pch=20,
     col="darkred",
     axes=F)
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26))# The bottom axis
axis(2, at=c(0,20,40,60,80,100,120,140,160,180,200,220)) # The left axis


########################################
# FITTING A LINEAR MODEL TO THE DATA 
########################################

model.pizza = lm(Sales ~ Population, data=pizza) 
# lm(Outcome V ~ Explanatory V, data=the name of data)

# Once you run an lm() function, use summary() to see the results.
summary(model.pizza)

# Based on the results, you need to report the following:
# What's the meaning of the coefficient for Population?
# Is the coefficient statistically signifcant at the 0.05 level?
# Is the coefficient statistically signifcant at the 0.01 level?
# Is the size of effect substantively large?

# Confidence interval
# By default, 95% CI
confint(model.pizza)
# Use the level argument to specify the CI level.
confint(model.pizza, level=.99)

########################################
# ADDING THE LINEAR LINE TO THE SCATTERPLOT 
########################################
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     xlim= c(0,26), ylim=c(0,220),
     pch=20,
     col="darkred",
     axes=F)
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26))# The bottom axis
axis(2, at=c(0,20,40,60,80,100,120,140,160,180,200,220)) # The left axis
# And then, add the line produced by the lm command using the abiline() function.
abline(model.pizza)

# Well, the color of the regression line in the slides is greenish, maybe darkgreen?
# Do you want to draw the same colored line? Sure, it's easy.
plot(pizza$Population, 
     pizza$Sales, 
     xlab="Student Population (1000s)", 
     ylab="Quarterly Sales ($1000s)",
     xlim= c(0,26), ylim=c(0,220),
     pch=20,
     col="darkred",
     axes=F)
axis(1, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26))# The bottom axis
axis(2, at=c(0,20,40,60,80,100,120,140,160,180,200,220)) # The left axis
abline(model.pizza, col="darkgreen")




######################################
# Exercise - Percent under 21 and fatal accidents
######################################

## Research Question: Are young drivers more likely to have fatal accidents?
## Import the Safety.csv data from 42 cities and answer the following questions.

# 1. Draw a bivariate plot to summarize the data. Place your explanatory and outcome variables properly.

# 2. Run a linear regression model and answer the following:

# a) What's the meaning of the coefficient for the explanatory variable?
# b) Is the coefficient statistically signifcant at the 0.05 level?
# c) Is the coefficient statistically signifcant at the 0.01 level?
# d) Is the size of effect substantively large?


