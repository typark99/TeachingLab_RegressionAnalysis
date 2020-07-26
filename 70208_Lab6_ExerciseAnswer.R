
################################################
# Exercise 1
# 1. Set Clinton as the baseline, and run the analysis again.
# 2. The relative risk ratio for moving from a full time worker to 
# an unemployed person for voting for Trump rather than Clinton?
# 3. The relative risk ratio for one-unit increase in income
# for voting for Trump rather than Clinton?
# 4. Predicted probabilities for a voter whose income level is 3
# and employment status is unemployed?
# 5. What's the effect of moving from the voter whose income level is 3
# and employment status is unemployed to another voter whose income level
# is 3 and employment status is fulltime?
# 6. test statistic for the impact of moving from full time to part time on the prob of
# voting for Johnson rather than Clinton?
# 7. p-value for the impact of income on the prob of
# voting for Trump rather than Clinton?
################################################

cces=read.csv("cces1.csv")
cces$pvote3 = relevel(cces$pvote3, ref = "Clinton")

# Create dummy variables for employment status, and set fulltime as the baseline.
cces$unemployed = rep(0, nrow(cces))
cces$unemployed
cces$unemployed[cces$employment=="unemployed"] = 1
cces$unemployed

cces$parttime = rep(0, nrow(cces))
cces$parttime
cces$parttime[cces$employment=="parttime"] = 1
cces$parttime

cces$fulltime = rep(0, nrow(cces))
cces$fulltime
cces$fulltime[cces$employment=="fulltime"] = 1
cces$fulltime

# 1.
# Run a multinomial logistic model
vote.2 = multinom(pvote3~income+unemployed+parttime, data=cces)
summary(vote.2)

# 2.
exp(0.16780732)

# 3.
exp(0.007413659)

# 4. 
newdata.5 = data.frame(income = 3, unemployed = 1, parttime=0)
y.pred.5 = predict(vote.2, newdata = newdata.5, type="probs") 

# 5.
newdata.6 = data.frame(income = 3, unemployed = 0, parttime=0)
y.pred.6 = predict(vote.2, newdata = newdata.6, type="probs") 
y.pred.6-y.pred.5

# 6. 
estimate = summary(vote.2)$coefficients
std.error = summary(vote.2)$standard.errors
z.stat = estimate/std.error

# 7.
2*pnorm(z.stat[2,2], lower.tail = F)


################################################
# Exercise 2
# Suppose you realize that it's important to control for gender and party affiliation
# to estimate the impacts of income and employment status accurately.
# Run a new model to take into account your new theory.
# Set Clinton as the baseline category for the outcome variable. 
# Set Democrat as the baseline category for the party variable.
# Set fulltime as the baseline category for employment status.
################################################

# Create dummy variables for party affiliation.
cces$Republican = rep(0, nrow(cces))
cces$Republican
cces$Republican[cces$party=="Republican"] = 1
cces$Republican

cces$Democrat = rep(0, nrow(cces))
cces$Democrat
cces$Democrat[cces$party=="Democrat"] = 1
cces$Democrat

cces$Independent = rep(0, nrow(cces))
cces$Independent
cces$Independent[cces$party=="Independent"] = 1
cces$Independent

vote.3 = multinom(pvote3~income+unemployed+parttime+female+Republican+Independent, data=cces)
summary(vote.3)

newdata.7 = data.frame(income = 5, unemployed = 0, parttime=1, female=1, Republican=0, Independent=0)
y.pred.7 = predict(vote.3, newdata = newdata.7, type="probs") # props type is for the multinom function


################################################
# Exercise 3
# Suppose your research hypothesis is that marital status is affected by
# people's perception about their work-life balance. 
# To estimate the relationship you will control for 
# age, gender, and job satisfaction. 
#
# Import Job.csv data. Run a regression model to test your hypothesis
# using the following variables:
# MaritalStatus: Single/Married/Divorced (Set Single as the baseline.)
# WorkLifeBalance: Numerical values; The higher, the more balanced (treat this as continuous)
# Age: Numerical values
# Gender: Female/Male (Create a dummy variable such that Female = 1; Male = 0)
# JobSatisfaction: Numerical values; The higher, the more satisfied (treat this as continuous)
################################################

job = read.csv("Job.csv")

# Set Single as the baseline.
job$MaritalStatus = relevel(job$MaritalStatus, ref = "Single")

# Create dummy variables for marital status.
job$Female = rep(0, nrow(job))
job$Female
job$Female[job$Gender=="Female"] = 1
job$Female

# Run a multinomial regression
job.model = multinom(MaritalStatus ~ WorkLifeBalance + Age + Female + JobSatisfaction, data=job)
summary(job.model)



################################################
# Exercise 4
# Based on the model from Exercise 3, answer the following questions.
#
# 1. What's the effect of a one level increase in the perception of work-life balance
# on the log odds of being Married rather than Single?
# 2. What's the effect of a one level increase in job satisfaction
# on the log odds of being Divorced rather than Single?
# 3. The relative risk ratio for moving from a man to 
# a woman for being Married rather than Single?
# 4. The relative risk ratio for a one level increase in the perception of work-life balance
# for being Married rather than Single? 
# 5. Predicted probability for being Single for a male who is 40 years old, 
# work-life balance level is 1 and job satisfaction level is 3?
# 6. test statistic for the impact of job satisfaction on the prob of
# being Divorced rather than Single?
# 7. p-value for the impact of the perception of work-life balance on the prob of
# being Married rather than Single?
# 8. Build a 95% Confidence interval for the effect of a one level increase in the perception of work-life balance
# on the log odds of being Married rather than Single.
################################################

# 1.
# -0.03527500

# 2. 
# -0.06291717

# 3. 
exp(-0.1039841)

# 4.
exp(-0.03527500)

# 5. 
newdata.7 = data.frame(Female = 0, Age = 40, WorkLifeBalance = 1, JobSatisfaction=3)
y.pred.7 = predict(job.model, newdata = newdata.7, type="probs") 
# 0.2721455

# 6. 
estimate = summary(job.model)$coefficients
std.error = summary(job.model)$standard.errors
z.stat = estimate/std.error
# -0.9578508

# 7.
2*pnorm(z.stat[2,2], lower.tail = T)
# 0.6820119

# 8. 
confint(job.model)
# [-0.20401910, 0.13346911]