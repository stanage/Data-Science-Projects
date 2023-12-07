# In this project, I try to find what categories of people have good credit scores, as shown by their individual Rating(s).
# First, I determine the significant factors by building a regression model and finding what factors have the most effects on the regression model. 
# This was done using the p-value of each factor. The accuracy of this model was checked using R_square on in-sample data.
# After finding the fcators with the most effect on individuals credit scores, I used those for categorizing the different individuals using hierarchical clustering.

rm(list=ls(all=T))

#load the data
data = read.table('DATA_3.01_CREDIT.csv', sep = ',', header = T)
str(data)
summary(data)

hist(data$Rating)
cor(data[,c(1:5,10)])

#========= Regression modeling to determine the significant factors
linreg=lm(Rating~., data=data, )
cor(linreg$fitted.values, data$Rating)
summary(linreg)
plot(data$Rating, linreg$fitted.values, xlab = "Actual", ylab = "Predicted")
plot(data$Income, data$Rating)


#========== Clustering with only the significant variables
data=data[,c(1:2,4,10)]
testdata=scale(data)
d=dist(testdata, method = 'euclidean')
hcward = hclust(d, method = 'ward.D')
plot(hcward)
data$groups<-cutree(hcward, k=5)
data

aggdata=aggregate(.~ groups, data=data, FUN = median)
proptemp=aggregate(Rating~ groups, data=data, FUN=length)
aggdata$proportion<-(proptemp$Rating)/sum(proptemp$Rating)
aggdata=aggdata[order(aggdata$proportion,decreasing=T),]
aggdata
