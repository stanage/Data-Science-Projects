# In this project, we analyze a dataset to see in what month of the year we have
# the maximum sales of ice cream.

# Clean the memory
rm(list=ls())

# Load the data
data=read.table('DATA_4.04_CHOC.csv',sep=',',header=TRUE)

# Summary statistics
str(data)
summary(data)

plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales", ylim=c(0,max(data$sales*1.2)), type='l')
regres=lm(sales~month,data=data)
summary(regres)

# Boxplot
boxplot(data$sales~ data$month, main="Chocolate sales by month",xlab="Month",ylab="Monthly sales", ylim=c(0,max(data$sales*1.2)))

# Recovery thanks to the model:
plot(data$time,data$sales,main="Chocolate sales over time",xlab="Time (in month)",ylab="Monthly sales",ylim=c(0,max(data$sales)*1.2),type='l')
lines(data$time,regres$fitted.values,type='l',col='blue',lty=2)
legend("topleft",c("Actual sales","Sales by the model"),lty=c(1,2),col=c('black','blue'))
                                                              
                                                              