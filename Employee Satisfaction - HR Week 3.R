# In this exercise, we build on the classification model that was used in a previous 
# example on HR analytics. Here, we used the predict function on a new dataset to 
# determine what employees are likely to leave the company. The result provides us with 
# a tool to decide on which employees in the new dataset we should work on retaining, 
# based on a priority score which we created by multiplying their predicted probabilities 
# to leave with their performances. 

# Clear up the memory
rm(list=ls(all=T))

# Load the data
dataold = read.table('DATA_3.02_HR2.csv', sep = ',', header = T)
datanew=read.table('DATA_4.02_HR3.csv', header = T,sep=',')

# Perform some summary statistics to get some insight into the data
str(dataold)
str(datanew)
summary(dataold)
summary(datanew)

# Create a classification model and predict outcome on new data
logreg = glm(left~., family=binomial(logit), data=dataold)
probtoleave = predict(logreg, newdata = datanew, type='response')


predattrition = data.frame(probtoleave)
View(predattrition)

predattrition$performance=datanew$LPE
View(predattrition)

plot(predattrition$probtoleave,predattrition$performance)
predattrition$priority = predattrition$probtoleave * predattrition$performance
View(predattrition)

orderpredattrition = predattrition[order(predattrition$priority, decreasing = T),]
View(orderpredattrition)


