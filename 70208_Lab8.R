################
# File: Lab8.R
# Author: Taeyong Park 
# Summary: Regression Analysis for Time Series Data
#################

# install.packages("tseries")
# install.packages("lmtest")
# install.packages("Hmisc")

library(tseries); library(lmtest); library(Hmisc)

top=read.csv("top1.csv") 
colnames(top)

# Test for stationarity 
adf.test(top$top1_cg, k=1)
adf.test(top$topmarg, k=1)
adf.test(top$capgtax, k=1)
adf.test(top$tbill_3, k=1)

# If every variable is stationary, 
# you can run a linear regression using the OLS (ADL).
# model = lm(top1_cg~topmarg+capgtax+tbill_3,data=top)

# The problem is that it's rare that every variable in the model is stationary.
# If you find evidence of nonstationarity, consider the ecm.


# First-difference
top$d.top1_cg = c(NA, diff(top$top1_cg, differences = 1))
top$d.topmarg = c(NA, diff(top$topmarg, differences = 1))
top$d.capgtax = c(NA, diff(top$capgtax, differences = 1))
top$d.tbill_3 = c(NA, diff(top$tbill_3, differences = 1))

# Create lagged variables
top$l.top1_cg = Lag(top$top1_cg)
top$l.topmarg = Lag(top$topmarg)
top$l.capgtax = Lag(top$capgtax)
top$l.tbill_3 = Lag(top$tbill_3)

# Cointegration test
step1 = lm(top1_cg~ topmarg + capgtax + tbill_3, data=top)
res.step1 = step1$residuals
step2 = adf.test(res.step1, k=1)

# ECM
model.ecm = lm(d.top1_cg ~ l.top1_cg + d.topmarg + l.topmarg + d.capgtax + l.capgtax + d.tbill_3 + l.tbill_3, data=top)
summary(model.ecm)

# Test for autocorrelated residuals
bgtest(model.ecm)



