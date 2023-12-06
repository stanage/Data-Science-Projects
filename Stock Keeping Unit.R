# This project is concerned with determining the stock keeping unit (SKU) of products. 

# Description: A firm produces many products but has limited storage space. 
# Hence they need to produce just the right amount of a particular product at any time, 
# based on how much of that product they sell on average in a day, (the Average Daily 
# Sales - ADS), and their level of certainty that they will sell that product 
# (the Coefficient of Variation - CV).

# Task: To advise the firm on what products to produce to stock and what products to 
# produce only when orders are made



rm(list=ls(all=TRUE))
data=read.table('DATA_2.01_SKU.csv', header=T, sep=',')
str(data)
summary(data)

# Plotting the data points
plot(data$CV, data$ADS, main='SKU Example', xlab = 'Coefficient of Variation', ylab = 'Average Daily Sales')
abline(v=0.2, col='red')
abline(h=4, col='red')
text(0.1,5, 'Horses', col='red')
text(0.6,5, 'Bulls', col='red')
text(0.8,3, 'Horses', col='red')


# Using hierarchical clustering and visualizing
testdata=data

testdata=scale(testdata)

d=dist(testdata, method = 'euclidean')

hcward=hclust(d, method = 'ward.D')

data$groups<-cutree(hcward, k=3)

library(lattice)

xyplot(ADS~CV, data=data, groups=groups, main='Cluster', type="p",
       auto.key = list(title="Group", space = "left", cex=1.0, just = 0.95),
       par.settings = list(superpose.line=list(pch = 0:18, cex=1)))



# ================== RESULTS =============================
# From the result, there are 3 clusters (categories), 
# 1) Products with high average daily sale and low variation. 
# Advice: These should be made to stock

# 2) Products with low average daily sales and high variation.
# Advice: These should be made to order

# 3) Products with high average daily sales and high variation.
# Advice: These products make a lower fraction, and should be made on a case-by-case bases
