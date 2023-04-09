#read in csv file
Lab1 <- read.csv(file = 'Lab1.csv')
# see summary statistics for EARN
summary(Lab1$EARN)
#see frequencies for Job.class
ftable(Lab1$Job.class)
#three way cross tabulation of the proportions of variables Educational level, Gender and Job.class
prop.table(ftable(Lab1$EDUC, Lab1$Gender, Lab1$Job.class))
#Histogram of earn
hist(Lab1$EARN)
#boxplot of EARN by Job.class
boxplot(Lab1$EARN~Lab1$Job.class)
#new variable EARNx10000 equal to Earnings divided by 10,000
Lab1$EARNx10000 = Lab1$EARN / 10000
#scatterplot with EARNx10000 on the x axis and AGE on the Y axis
plot(Lab1$EARNx10000, Lab1$AGE)