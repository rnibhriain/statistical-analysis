survey <- read.csv("survey.csv")

tbl <- table(survey$Smoke, survey$Exer)
chisq.test(tbl)

# Pearson's Chi-squared test
# 
# data:  tbl
# X-squared = 5.4885, df = 6, p-value = 0.4828




# 1)	How were the degrees of freedom for this test calculated?

#     the number number of rows - 1 by the number of columns - 1

# 2)	What are the assumptions of the chi square test?

#     all observations must contribute to only one cell
#     in a two by two table all expected values must be > 5
#     in a larger table n > 20, all expected values must be > 1 and no more 
#     than 20% of expected values can be < 5

# 3)	Why did the code give an error message?
  
#     the expected values are very small and the approximation may be poor as the test statistic relies on normal distribution

# 4)	What is the null hypothesis for this test and the alternative hypothesis for this test?

#     H0: is that no relationship exists on the categorical variables of excercise and smoking in the population
#     i.e. they are independent 
#     HA: is that a relationship exists on the categorical variables of excercise and smoking in the population
#     i.e. they are not independent

# 5)	Using a significance level of 0.05, give your conclusions for the result of this test.

#     the pvalue is 0.2828 which is greater than 0.05, therefore we can fail to reject the Null Hypothesis



