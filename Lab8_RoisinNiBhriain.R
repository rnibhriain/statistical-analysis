# 1) Plot the residuals density

faithful.lm <- lm(eruptions ~ waiting, data=faithful)  # build linear regression model on full data
print(faithful.lm)

#compute residuals
faithful.res <- resid(faithful.lm)

# plot the residuals against the observed values
plot(faithful$eruptions, faithful.res, ylab="Residuals", xlab="eruptions", 
     main="Faithful Linear Model") 
abline(0, 0)                  # the horizon

plot(faithful$waiting, faithful.res, ylab="Residuals", xlab="waiting", 
     main="Faithful Linear Model") 
abline(0, 0)                  # the horizon

# inspecting the residuals density
plot(density(faithful.res), main="Density Plot: residuals", ylab="Frequency") 


# 2)	Use the plot function to generate 4 graphs of the residuals 
# vs fitted values, etc. in a single plot

par(mfrow=c(2,2))
plot(faithful.lm)


# 3)	Comment on whether or not you feel the model is appropriate, 
# given both what you have seen this week in terms of the residuals 
# plots, and what you saw last week in terms of the results of the
# model. Particularly, comment on the appearance of the density
# plots of the two variables that you made last week as compared 
# to the density plots of the residuals.

# It is not an appropriate model due to the fact that the density plots base
# on both of the variables are different. The N is the same as last weeks N
# However the bandwidth for last weeks variables were 0.3348 for eruptions
# and 3.988 for waiting, this week's was 0.1454. Since these bandwidths
# are so different, it makes them difficult to compare. 

# Last weeks density plots had a large dip in the middle whereas this weeks density
# plot has almost no dip and is just a peak at about 0 in the x. With a small dip at about -0.4 in the x.

# The residuals vs fitted graph does not show us a straigt red line which indicates
# that there is a problem

# The Q-Q plot shows a straight line for the theoretical quantities vs the standardized residuals
# most of the points are near this line also which is a good sign

# The Scale - Location Plot of fitted vs standardized residuals shows a curved red line
# which also indicates that there is a problem

# Finally, the Residuals Vs Leverage Plot - there is no red dotted line on the graph which indicates that all the points are 
# within range.

