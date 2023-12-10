# In this project on HR analytics, the aim is to gain insight into the number of 
# people leaving a company, to advice management on what steps to take.
# First, a logistic regression model is used, with "Left" column as the target,
# and carrying values, 1 and 0, for those who left and those who didn't leave respectively.
# With the model, two things are determined: 1) A ranking of the effects of all factors
# on employee attrition. 2) The nature of the effect of a factor on employee attrition,
# whether positive or negative.


# ======== Inferences
# 1) From this, it can be seen that the time spent in the company is an important factor affceting
# attrition, and has a positive effect on it. It can also be seen that the effect falls under the
# increasing trend for 6 years, but that it is a smaller fraction of individuals that make up that
# category. Now, it is worth looking into this group - perhaps it's an outlier, perhaps it's not.
# 
# 2) It can also be seen that satisfaction has the most effect on attrition, and has a negative 
# effect on it; meaning the more satisfied an employee, the higher the chances of them leaving the
# company. Now this should be a cause for concern. Perhaps these employees are burned out, or have
# to terminate their employements based on a contract.


rm(list = ls(all=TRUE))

#load the dataset
dataset=read.table("../Week 2/DATA_3.02_HR2.csv", header = T, sep = ",")

# Now lets look at the dataset
str(dataset)
summary(dataset)

table(dataset$left)
table(dataset$left)/nrow(dataset)
hist(dataset$left)


#================ Building a Logistic Regression Model
cor(dataset) # correlation is not a good way of understanding how factors affect outcome so its best to use a model

logreg=glm(left~., family = binomial(logit), data = dataset)

hist(logreg$fitted.values)

cor(dataset$left, logreg$fitted.values)

# ============== Checking Model Performance with Different Cutoff Values
cutoff = 0.5

sum((logreg$fitted.values<=cutoff)&(dataset$left==0))/sum(dataset$left==0) # Compute the percentage of correctly classified employees who stayed
sum((logreg$fitted.values>cutoff)&(dataset$left==1))/sum(dataset$left==1) # Compute the percentage of correctly classified employees who left
mean((logreg$fitted.values>cutoff)==(dataset$left==1)) # Compute the overall percentage of correctly classified employees

table(logreg$fitted.values >= cutoff, dataset$left)


#===========================
cutoff = 0.5
table(logreg$fitted.values >= cutoff, dataset$left)
mean((logreg$fitted.values>cutoff)==(dataset$left==1))


cutoff = 0.7
table(logreg$fitted.values >= cutoff, dataset$left)
mean((logreg$fitted.values>cutoff)==(dataset$left==1))


cutoff = 0.3
table(logreg$fitted.values >= cutoff, dataset$left)
mean((logreg$fitted.values>cutoff)==(dataset$left==1))
#===========================

# Finding the effects of factors on target from model
summary(logreg)


# Visualizing how Time in Company (TIC) affects attrition
plot(dataset$TIC,dataset$left,main= "Time and Employee Attrition", ylab="Attrition", xlab= "Time spent")


# The above plot doesn't give a good visualization, so we find the mean of attrition 
# at every time spent in the company, and make that visulaization
tempdata=dataset

aggbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=mean) # We compute the average attrition rate for each value of TIC

plot(aggbTimeRank$TIC,aggbTimeRank$left,main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent")



# But the above plot doesn't show how much attrition makes an average. Now that is implemented

cntbTimeRank=aggregate(left~ TIC, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbTimeRank$TIC,aggbTimeRank$left,circles=cntbTimeRank$left, inches=0.6, fg="white", bg="red",main= "Time and Employee Attrition", ylab="Average Attrition Rate", xlab= "Time spent") # we




# Let's use a more visual way to see the effect of the most important driver: Satisfaction

tempdata=dataset

tempdata$rankSatis = round(rank(-tempdata$S)/600) # We create categories of employee satisfaction ranking. We create 20 groups (because it will work well later...)

aggbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=mean) # We compute the average attrition rate for each category

cntbSatisRank = aggregate(left~ rankSatis, data=tempdata, FUN=length) # We compute the number of employees for each value of TIC

symbols(aggbSatisRank$rankSatis,aggbSatisRank$left,circles=cntbSatisRank$left, inches=.2, fg="white", bg="red",main= "Satisfaction and Employee Attrition", ylab="Average Attrition Rate", xlab= "Rank of Satisfaction")


