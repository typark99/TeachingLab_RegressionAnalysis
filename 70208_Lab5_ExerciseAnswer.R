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
# to another customer who spent 4000 when both have a Simmons credit card?
######################

newdata.1 = data.frame(Spending=3, Card=1)
y.pred.1 = predict(logistic.simmons, newdata=newdata.1, type="response")

newdata.2 = data.frame(Spending=5, Card=0)
y.pred.2 = predict(logistic.simmons, newdata=newdata.2, type="response")

newdata.3 = data.frame(Spending=5, Card=1)
y.pred.3 = predict(logistic.simmons, newdata=newdata.3, type="response")
y.pred.3 - y.pred.2

newdata.4 = data.frame(Spending=4, Card=1)
y.pred.4 = predict(logistic.simmons, newdata=newdata.4, type="response")
y.pred.4 - y.pred.1

#######################
# Exercise 2
#
# 1. What is the impact on the odds of changing from 0 to 1 in the Card variable,
# while the Spending variable is held constant?
# 2. What is the impact on the odds of changing from 0 to 1 in the Spending variable,
# while the Card variable is held constant?
#######################
exp(1.0987)
exp(0.3416)

## Odds ratio
exp(coef(logistic.simmons))


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
lake = read.csv("Lakeland.csv")

lake.model = glm(Return~GPA+Program, data=lake, family=binomial)
summary(lake.model)

# 1 
newdata.1 = data.frame(GPA=3.5, Program=0)
y.pred.1 = predict(lake.model, newdata=newdata.1, type="response")

newdata.2 = data.frame(GPA=3.5, Program=1)
y.pred.2 = predict(lake.model, newdata=newdata.2, type="response")

y.pred.2-y.pred.1

# 2 
newdata.3 = data.frame(GPA=2.5, Program=1)
y.pred.3 = predict(lake.model, newdata=newdata.3, type="response")

newdata.4 = data.frame(GPA=3.5, Program=1)
y.pred.4 = predict(lake.model, newdata=newdata.4, type="response")

y.pred.4-y.pred.3

# 3. Odds ratio
exp(coef(lake.model))

