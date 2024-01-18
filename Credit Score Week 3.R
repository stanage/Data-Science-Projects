# In this exercise, we built on what was done previously. We used the linear regression
# model to predict the credit scores of different individuals to determine if they should 
# be offered loans or not, based on a list of variables such as age, student etc
# We then tested our model on an out-of-sample dataset, to see how well the model 
# generalises, and we found that it performed well even on the new dataset. 

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# Load the data
dataold = read.table('DATA_3.01_CREDIT.csv', sep=',', header = T)
datanew = read.table('DATA_4.01_CREDIT2.csv', sep=',', header = T)

# Now lets do some summary statistics
str(dataold)
str(datanew)
summary(dataold)
summary(datanew)

# Create a linear regression model
linreg = lm(Rating~., data=dataold)
predcreditscore = predict(linreg, newdata=datanew, type='response')

cor(linreg$fitted.values, dataold$Rating)
plot(dataold$Rating, linreg$fitted.values)

cor(predcreditscore,datanew$Rating)
plot(datanew$Rating, predcreditscore)
