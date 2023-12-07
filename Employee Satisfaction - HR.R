# In this project, we categorize employees into different groups based on data collected from them during exit interviews, with the following features:

#S: Satisfaction level
#LPE: Last project evaluation
#NP: Number of projects completed in last twelve months
#ANH: Average number of hours worked in last twelve months
#TIC: Time spent in the company
#Newborn: Number of babies had in the last twelve months

# We form some categories that describe the different employees who are leaving the company, in doing that, we identify categories whose exit would be a loss to the company, those whose exit benefit the company, or others who can be retained if certain actions where taken, and we advise the 


rm(list=ls(all=TRUE))

# load dataset
data=read.table('DATA_2.02_HR.csv', header = T, sep = ',')
str(data)

testdata=scale(data)
d=dist(testdata, method = 'euclidean')
hcward = hclust(d, method = 'ward.D')
plot(hcward)
data$groups<-cutree(hcward, k=4)

aggdata=aggregate(.~ groups, data=data, FUN = mean)

proptemp=aggregate(S~ groups, data=data, FUN=length)
aggdata$proportion<-(proptemp$S)/sum(proptemp$S)

aggdata=aggdata[order(aggdata$proportion,decreasing=T),]


aggdata

#==== Remove Newborn variable. This is done because there's little or no action the company can take to affect this, hence it's not an actionable business variable

rm(list=ls(all=TRUE))
# load dataset
data=read.table('DATA_2.02_HR.csv', header = T, sep = ',')
testdata=data[,1:5]
str(testdata)
testdata=scale(testdata)
d=dist(testdata, method = 'euclidean')
hcward = hclust(d, method = 'ward.D')
plot(hcward)
data$groups<-cutree(hcward, k=4)

aggdata=aggregate(.~ groups, data=data, FUN = median)

proptemp=aggregate(S~ groups, data=data, FUN=length)
aggdata$proportion<-(proptemp$S)/sum(proptemp$S)

aggdata=aggdata[order(aggdata$proportion,decreasing=T),]


aggdata
