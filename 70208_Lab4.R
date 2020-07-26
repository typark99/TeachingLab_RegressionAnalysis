################
# File: Lab4.R
# Authors: Taeyong Park 
# Summary: Categorical independent variables
#################


#########################
# RestaurantRatings data
#########################
# The Consumer Reports Restaurant Customer Satisfaction Survey
# studies full-service restraurant chanis. 
# One of the variables in the study is meal price, the average amount paid per
# person for dinner and drinks, minus the tip. 
# The RestaurantRatings3.csv data show the meal prices obtained from 24 restaurants
# in the Grand Strand section in a city of the US.

ratings = read.csv("RestaurantRatings3.csv")
colnames(ratings)
class(ratings$Price)
class(ratings$Type) # The nature of factor variables is categorical.
table(ratings$Type)

# Create dummy variables for the Type variable

ratings$Type.Italian = rep(0, nrow(ratings))
ratings$Type.Italian
ratings$Type.Italian[ratings$Type=="Italian"] = 1
ratings$Type.Italian

ratings$Type.Seafood = rep(0, nrow(ratings))
ratings$Type.Seafood
ratings$Type.Seafood[ratings$Type=="Seafood"] = 1
ratings$Type.Seafood

ratings$Type.Steakhouse = rep(0, nrow(ratings))
ratings$Type.Steakhouse
ratings$Type.Steakhouse[ratings$Type=="Steakhouse"] = 1
ratings$Type.Steakhouse

model.ratings = lm(Score ~ Price + Type.Seafood + Type.Steakhouse, data=ratings)
summary(model.ratings)

######################
# Google-form exercise
######################

# Go to bit.ly/3d6Eg0g

##################
# MutualFunds data
##################
mutual = read.csv("MutualFunds.csv")
colnames(mutual)

class(mutual$Fund.Type) # Factors in R are stored as a vector of integer values with a corresponding set of character values to use when the factor is displayed
table(mutual$Fund.Type) # Three categories

class(mutual$Morningstar.Rank) # Factors in R are stored as a vector of integer values with a corresponding set of character values to use when the factor is displayed
table(mutual$Morningstar.Rank) # Four categories

## Create dummy variables for the Fund.Type variable.

mutual$Fund.Type.DE = rep(0, nrow(mutual))
mutual$Fund.Type.DE
mutual$Fund.Type.DE[mutual$Fund.Type=="DE"] = 1
mutual$Fund.Type.DE

mutual$Fund.Type.FI = rep(0, nrow(mutual))
mutual$Fund.Type.FI
mutual$Fund.Type.FI[mutual$Fund.Type=="FI"] = 1
mutual$Fund.Type.FI

mutual$Fund.Type.IE = rep(0, nrow(mutual))
mutual$Fund.Type.IE
mutual$Fund.Type.IE[mutual$Fund.Type=="IE"] = 1
mutual$Fund.Type.IE

# Baseline category: DE
model.mutual.1 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.IE + Fund.Type.FI, data=mutual)
summary(model.mutual.1)


##################
# Prediction
##################

# Question: What is the predicted value of 5-Y avg return for a domestic equity fund 
# whose net asset value is 30 and expense ratio is 2%?
# We calculated such predicted values in the lecture.
# In R, you can use the predict function. 
# First, you need to create new data: Net.Asset.Value = 30, Expense.Ratio = 2, Fund.Type = DE
newdata.1 = data.frame(Net.Asset.Value = 30, Expense.Ratio = 2, Fund.Type.IE = 0,  Fund.Type.FI = 0)
y.pred.1 = predict(model.mutual.1, newdata=newdata.1)

# New data: Net.Asset.Value = 35, Expense.Ratio = 3, Fund.Type = IE
newdata.2=data.frame(Net.Asset.Value = 35, Expense.Ratio = 3, Fund.Type.IE = 1,  Fund.Type.FI = 0)
y.pred.2 = predict(model.mutual.1, newdata=newdata.2)

#######################
# Exercise 1
#
# 1. Run the model with the baseline category IE. Name the model model.mutual.2 
# 2. Run the model with the baseline category FI. Name the model model.mutual.3
######################


########################
# Exercise 2
#
# What is the predicted value of 5-Y avg return for an international equity fund 
# whose net asset value is 30 and expense ratio is 1%?
#
# 1. Use model.mutual.1 (Baseline: DE)
# 2. Use model.mutual.2 (Baseline: IE)
# 3. Use model.mutual.3 (Baseline: FI)
######################


#######################
# Exercise 3
#
# Create dummy variables for Morningstar.Rank using the following code
# and run a linear regression model with the baseline category of Morningstar.Rank = 3-Star.
# baseline fund.type is DE
######################
mutual$Morningstar.Rank.2Star = rep(0, nrow(mutual))
mutual$Morningstar.Rank.2Star
mutual$Morningstar.Rank.2Star[mutual$Morningstar.Rank=="2-Star"] = 1
mutual$Morningstar.Rank.2Star



########################
# Exercise 4
#
# Run the model with the baseline category of Morningstar.Rank = 5-Star and the baseline category IE.
######################

########################
# Exercise 5
#
# What is the predicted value of 5-Y avg return for an international equity fund 
# whose net asset value is 27 and expense ratio is 1%?
######################

