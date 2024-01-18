# In this project, we use survival analysis for predictive maintenance, to determine 
# the expected lifetime of different equipment based on pressure Index, moisture Index, 
# temperature Index, team servicing the equipment, and provider of the equipment. We didn't 
# use a linear regression model to predict lifetime of equipment because for those 
# equipment in the dataset which are still working, their full lifetime is unknown, hence,
# the model will not be accurate. We used the survreg function instead, with the dependent
# variable being a new variable that combines lifetime and current status if broken or not.
# After creating the model, we determined the predicted median survival time of each 
# equipment, stored in the Ebreak variable.

# Clean the memory
rm(list = ls(all=T))

# Load the data
data=read.table('DATA_4.03_MNT.csv',sep=',',header=TRUE)

str(data)
summary(data)

# Try a linear regression model. Although we aleady know that this is not the best 
# model for this problem
linreg = lm(lifetime~.-broken, data=data)
summary(linreg)

install.packages("survival")
library(survival)


dependantvars = Surv(data$lifetime, data$broken)
survreg = survreg(dependantvars~pressureInd+moistureInd+temperatureInd+team+provider, dist="gaussian",data=data) 
summary(survreg)  

Ebreak=predict(survreg, newdata=data, type="quantile", p=.5)
Forecast=data.frame(Ebreak)
Forecast$lifetime=data$lifetime
Forecast$broken=data$broken
Forecast$RemainingLT=Forecast$Ebreak-data$lifetime
View(Forecast)

Forecast=Forecast[order(Forecast$RemainingLT),]
View(Forecast)
ActionsPriority=Forecast[Forecast$broken==0,]
View(ActionsPriority)
