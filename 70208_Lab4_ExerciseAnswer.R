mutual = read.csv("MutualFunds.csv")

#######################
# Exercise 1
#
# 1. Run the model with the baseline category IE. Name the model model.mutual.2 
# 2. Run the model with the baseline category FI. Name the model model.mutual.3
######################

# Baseline category: IE
model.mutual.2 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.DE + Fund.Type.FI, data=mutual)
summary(model.mutual.2)

# Baseline category: FI
model.mutual.3 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.DE + Fund.Type.IE, data=mutual)
summary(model.mutual.3)


########################
# Exercise 2
#
# Make a prediction
# What is the predicted value of 5-Y avg return for an international equity fund 
# whose net asset value is 30 and expense ratio is 1%?
# 1. Use model.mutual.1 (Baseline: DE)
# 2. Use model.mutual.2 (Baseline: IE)
# 3. Use model.mutual.3 (Baseline: FI)
######################
# 1. Use model.mutual.1 (Baseline: DE)
model.mutual.1 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.IE + Fund.Type.FI, data=mutual)
newdata.1=data.frame(Net.Asset.Value = 30, Expense.Ratio = 1, Fund.Type.IE = 1,  Fund.Type.FI = 0)
y.pred.1 = predict(model.mutual.1, newdata=newdata.1)

# 2. Use model.mutual.2 (Baseline: IE)
model.mutual.2 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.DE + Fund.Type.FI, data=mutual)
newdata.2=data.frame(Net.Asset.Value = 30, Expense.Ratio = 1, Fund.Type.DE = 0,  Fund.Type.FI = 0)
y.pred.2 = predict(model.mutual.2, newdata=newdata.2)

# 3. Use model.mutual.3 (Baseline: FI)
model.mutual.3 = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.DE + Fund.Type.IE, data=mutual)
newdata.3=data.frame(Net.Asset.Value = 30, Expense.Ratio = 1, Fund.Type.DE = 0,  Fund.Type.IE = 1)
y.pred.3 = predict(model.mutual.3, newdata=newdata.3)


#######################
# Exercise 3
#
# Create dummy variables for Morningstar.Rank using the following code
# and run the model with the baseline category of Morningstar.Rank is 3-Star.
# and the baseline category DE.
######################
mutual$Morningstar.Rank.2Star = rep(0, nrow(mutual))
mutual$Morningstar.Rank.2Star
mutual$Morningstar.Rank.2Star[mutual$Morningstar.Rank=="2-Star"] = 1
mutual$Morningstar.Rank.2Star

mutual$Morningstar.Rank.3Star = rep(0, nrow(mutual))
mutual$Morningstar.Rank.3Star
mutual$Morningstar.Rank.3Star[mutual$Morningstar.Rank=="3-Star"] = 1
mutual$Morningstar.Rank.3Star

mutual$Morningstar.Rank.4Star = rep(0, nrow(mutual))
mutual$Morningstar.Rank.4Star
mutual$Morningstar.Rank.4Star[mutual$Morningstar.Rank=="4-Star"] = 1
mutual$Morningstar.Rank.4Star

mutual$Morningstar.Rank.5Star = rep(0, nrow(mutual))
mutual$Morningstar.Rank.5Star
mutual$Morningstar.Rank.5Star[mutual$Morningstar.Rank=="5-Star"] = 1
mutual$Morningstar.Rank.5Star


model.mutual.full.3star = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.IE + Fund.Type.FI + Morningstar.Rank.2Star + Morningstar.Rank.4Star + Morningstar.Rank.5Star, data=mutual)
summary(model.mutual.full.3star)

########################
# Exercise 4
#
# Run the model with the baseline category of Morningstar.Rank = 5-Star and the baseline category IE.
######################
model.mutual.full.5star = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.DE + Fund.Type.FI + Morningstar.Rank.2Star + Morningstar.Rank.3Star + Morningstar.Rank.4Star, data=mutual)
summary(model.mutual.full.5star)

########################
# Exercise 5
#
# What is the predicted value of 5-Y avg return for a 4 star international equity fund 
# whose net asset value is 27 and expense ratio is 1%?
######################
model.mutual.full.3star = lm(Avg.Return.5Y ~ Net.Asset.Value + Expense.Ratio + Fund.Type.IE + Fund.Type.FI + Morningstar.Rank.2Star + Morningstar.Rank.4Star + Morningstar.Rank.5Star, data=mutual)
newdata = data.frame(Net.Asset.Value = 30, Expense.Ratio = 1, Fund.Type.IE = 1, Fund.Type.FI = 0, Morningstar.Rank.2Star = 0, Morningstar.Rank.4Star = 1, Morningstar.Rank.5Star = 0)
y.pred = predict(model.mutual.full.3star, newdata=newdata)

