################
# File: Lab5.R
# Author: Taeyong Park 
# Summary: Logistic regression
#################

simmons = read.csv("Simmons.csv")

#########################
# Visualize data 
#########################
plot(simmons$Spending,
     simmons$Coupon,
     pch=8, xlab="Spending", ylab="Using Coupon",
     main="Simmons Stores Data, Spending - Using Coupon",
     col="seagreen")


#########################
# Logistic regression
#########################
logistic.simmons = glm(Coupon~Spending+Card, data=simmons, 
                       family=binomial)
summary(logistic.simmons)


## Confidence interval
confint(logistic.simmons)
confint(logistic.simmons, level = .99)


## Predicted probabilities

# A question from the lecture slide:
# What is the predicted probability of using the coupon when the customer
# spends $2000 at Simmons and doesn't own a Simmons credit card?
exp(-1.462)/(1+exp(-1.462))

# We can also use the predict function in R.
newdata.1 = data.frame(Spending=2, Card=0)
y.pred.1 = predict(logistic.simmons, newdata=newdata.1, type="response")
# The type="response" option tells R to output probabilities of the form P(Y = 1|X)

# Another question from the lecture slide:
# What is the estimated effect on the probability of using the coupon 
# of moving from a customer who has a Simmons credit card to 
# a customer who doesn't, when the two customers spent $2000?

# Calculate the predicted probability of using the coupon when the customer
# spends $2000 at Simmons and owns a Simmons credit card.
exp(-.363)/(1+exp(-.363))
# And, calculate the difference
exp(-1.462)/(1+exp(-1.462)) - exp(-.363)/(1+exp(-.363))

# Equivalently, we can use the predict function.
newdata.2 = data.frame(Spending=2, Card=1)
y.pred.2 = predict(logistic.simmons, newdata=newdata.2, type="response")

y.pred.1 - y.pred.2



#######################
# Exercise 1
# Use the predict function to answer the following questions.
#
# 1. What is the expected probability of using the coupon for the customers 
# who spent 3000 and have a Simmons credit card?
#
# 2. What is the expected probability of using the coupon for the customers 
# who spent 5000 and don't have a Simmons credit card?
#
# 3. What is the expected effect of moving from a customer 
# who doesn't have a Simmons credit card 
# to another customer who has the card when both spent 5000?
#
# 4. What is the expected effect of moving from a customer who spent 3000
# to another customer who spent 4000 when both has a Simmons credit card?
######################

#######################
# Exercise 2
#
# 1. What is the impact on the odds of changing from 0 to 1 in the Card variable,
# while the Spending variable is held constant?
# 2. What is the impact on the odds of changing from 0 to 1 in the Spending variable,
# while the Card variable is held constant?
#
# Hint: use exp() function.
#######################




#######################
# Exercise 3
#
# Import Lakeland.csv and answer the following questions.
#
# 1. What is the estimated effect on the probability of returning to Lakeland 
# of moving from a student who didn't attend the orientation program to another student 
# who attended the orientation program when the two students' GPA is 3.5?
#
# 2. What is the estimated effect on the probability of returning to Lakeland 
# of moving from a student whose GPA is 2.5 to another student 
# whose GPA is 3.5 when both the students attended the orientation program?
#
# 3. Calculate the odds ratio for the GPA and Program variables.
######################### 



















