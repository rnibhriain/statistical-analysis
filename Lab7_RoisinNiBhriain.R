# 1)	Check the correlation between the variables eruptions and waiting
head(faithful)
cor(faithful$eruptions, faithful$waiting)

# 2)	Create a plot that shows both the points and a smoothed line of the points 
plot(faithful$eruptions, faithful$waiting)
scatter.smooth(x=faithful$eruptions, y=faithful$waiting, main="Eruptions ~ Waiting")

# 3)	Create box plots for the variables
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out))  # box plot for 'Eruptions'
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))  # box plot for 'Waiting'

# 4)	Create graphs of the densities of the variables
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")  # density plot for 'eruptions'
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency") # density plot for 'waiting'

# 5)	Fit a simple linear model that predicts eruptions from waiting
faithful.lm <- lm(waiting ~ eruptions, data=faithful)  # build linear regression model on full data
print(faithful.lm)
summary(faithful.lm)
plot(faithful$eruptions, faithful$waiting)

# 6)	Visualise the resulting regression line on a scatterplot of the data

abline(faithful.lm)

# 7)	In comments in your code, write an equation that describes the linear model you have fitted

#     Y = B0 +B1X1 +e
#     B0: the intercept: 33.4744
#     B1: the coefficient of the variable X1. The slope of the line: 10.7296 for eruptions
#     e: the error term: 5.914 

# 8)	At a significance level of 0.05, does there appear to be a statistically significant relationship between eruptions and waiting? Explain your answer in comments in your code. 

#     yes as the pvalue is 2.2e-16 which is less than 0.05. therefore you can accept the HA
#     that there is no relationship between eruptions and waiting as it is statistically significant.

# 9)	Why are the p-values for the variable waiting and the overall F test so similar for this model?

#     The regeression model fits the data better than the model with no independent variables as eruptions is the dependent variable.
