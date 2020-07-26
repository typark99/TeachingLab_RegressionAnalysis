#####################################
# Exercise 1: What determines income?
#
# 1) Develop a theory. In other words, choose
# your explanatory variable.
# 2) Build your model. In other words, choose
# your control variables.
# 3) Run a regression analysis.
#####################################
cces=read.csv("cces1.csv")
colnames(cces)

# One possible answer
# 1) Income is influenced by gender.
# 2) Control for education, employment status, race, and age.

class(cces$income)
table(cces$income)
# 3) Treat income as continuous, and run a linear regression.



# Create dummy variables for employment.
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


# Create dummy variables for education.
cces$highschool = rep(0, nrow(cces))
cces$highschool
cces$highschool[cces$education=="highschool"] = 1
cces$highschool

cces$bachelor = rep(0, nrow(cces))
cces$bachelor
cces$bachelor[cces$education=="bachelor"] = 1
cces$bachelor

cces$postgrad = rep(0, nrow(cces))
cces$postgrad
cces$postgrad[cces$education=="postgrad"] = 1
cces$postgrad

# Create dummy variables for race.
cces$black = rep(0, nrow(cces))
cces$black
cces$black[cces$race=="black"] = 1
cces$black

cces$latino = rep(0, nrow(cces))
cces$latino
cces$latino[cces$race=="latino"] = 1
cces$latino

cces$white = rep(0, nrow(cces))
cces$white
cces$white[cces$race=="white"] = 1
cces$white

# Create dummy variables for age.
cces$above65 = rep(0, nrow(cces))
cces$above65
cces$above65[cces$age=="above65"] = 1
cces$above65

cces$below30 = rep(0, nrow(cces))
cces$below30
cces$below30[cces$age=="below30"] = 1
cces$below30

cces$middle = rep(0, nrow(cces))
cces$middle
cces$middle[cces$age=="middle"] = 1
cces$middle

model.income = lm(income~female+parttime+fulltime+bachelor+postgrad+
                    black+latino+below30+middle, data=cces)
summary(model.income)  

  
  

#####################################
# Exercise 2: Import Job.csv and 
# take a look the data.
#
# The variable names are intuitive enough.
# Start with developing your own research question.
#
# 1) Develop a theory. In other words, choose
# your explanatory variable.
# 2) Build your model. In other words, choose
# your control variables.
# 3) Run a regression analysis.
#####################################
job = read.csv("Job.csv")
colnames(job)

# One possible answer
# 1) Job attrition is influenced by job statistifaction.
# 2) Control for age, gender, and distannce from home.

job$Female = rep(0, nrow(job))
job$Female
job$Female[job$Gender=="Female"] = 1
job$Female

model.job = glm(Attrition~JobSatisfaction+Age+Female+DistanceFromHome,
                data=job, family=binomial)
summary(model.job)

# Odds ratio
exp(coef(model.job))

# Predicted probabilities
newdata = data.frame(JobSatisfaction=2, Age=40, Female=0, DistanceFromHome=10)
y.pred = predict(model.job, newdata=newdata, type="response")

