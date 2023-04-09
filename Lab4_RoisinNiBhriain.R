# vector of size 100 with mean 4 and standard deviation 5 
vector <- rnorm(100, 4, 5)

# 1) H0: the mean of the population is equal to 0. HA: the mean of 
# the population is not equal to 0. Use a z-test with the true 
# population SD. use alpha = 0.05

##code for z-test
#Definitions:
#let var mu be the mean of the population under the null hypothesis
#let var n be the sample size
#let var x_bar be the mean of the sample
#let var sd be the known population sd , or our estimate of it from the sample
#let alpha be the significance level eg 0.05
#let string var tails be "two" "left" or "right" indicating a two-tailed test or direction of one-tailed


##code for a one-sample hypothesis test function
OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"), mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  #return the list
  return( ret )
}


OneSampTest("z", 'two', 0.05, 0, 100, 5, 4)

#ANSWER FOR QUESTION 1)


# $type
# [1] "One Sample z test. two tailed"
# 
# $mu
# [1] 0
# 
# $n
# [1] 100
# 
# $x_bar
# [1] 5
# 
# $se
# [1] 0.4
# 
# $p
# [1] 7.465129e-36
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "significant"

#Reject the HA


#2) H0: the mean of the population is greater than or equal to 
# 4.2. HA: the mean of the population is less than 4.2. Use a 
# t-test with the sample SD. use alpha = 0.025

OneSampTest("t", "right", 0.025, 4.2, 100, 4, 5)

#ANSWER FOR Q2
# $type
# [1] "One Sample t test. right tailed"
# 
# $mu
# [1] 4.2
# 
# $n
# [1] 100
# 
# $x_bar
# [1] 4
# 
# $se
# [1] 0.5
# 
# $p
# [1] 0.6549909
# 
# $alpha
# [1] 0.025
# 
# $significance
# [1] "not significant"

#Fail to Reject the H0

# 3) Repeat the previous task using the t.test function in the 
# package {stats}


install.packages('Rlab')
library(Rlab)
# new vector sample
vector1 <- rnorm(100, 4, 5)

t.test(vector, vector1)

#ANSWER FOR Q3

# Welch Two Sample t-test
# 
# data:  vector and vector1
# t = -1.192, df = 193.3, p-value = 0.2347
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.2299970  0.5499382
# sample estimates:
#   mean of x mean of y 
# 3.357045  4.197074 

# I'm not entirely sure what this means but i think that you fail to reject 
# the H0

# 4)	H0: the population proportion is equal to 0.28. HA: the population 
#proportion is not equal to 0.28. Use alpha = 0.05

#100 bernouilli trial vector

bVector <- rbern(100, 0.3)

#let prop be the sample proportion
#calculate sd for a z-test of proportions
sd = sqrt((prop)(1-prop))

OneSampTest("z", "two", 0.05, 0.28, 100, 0.3, sd)

# ANSWER FOR 4

# $type
# [1] "One Sample z test. two tailed"
# 
# $mu
# [1] 0.28
# 
# $n
# [1] 100
# 
# $x_bar
# [1] 0.3
# 
# $se
# [1] 0.5
# 
# $p
# [1] 0.9680931
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "not significant"

# Fail to Reject the H0

# 5) H0: the population proportion is less than or equal to 0.35. 
# HA: the population proportion is greater than 0.35. Use alpha = 0.05

OneSampTest("z", "left", 0.05, 0.35, 100, 0.28, sd)

# ANSWER FOR 5
# 
# $type
# [1] "One Sample z test. left tailed"
# 
# $mu
# [1] 0.35
# 
# $n
# [1] 100
# 
# $x_bar
# [1] 0.28
# 
# $se
# [1] 0.5
# 
# $p
# [1] 0.44433
# 
# $alpha
# [1] 0.05
# 
# $significance
# [1] "not significant"

# Fail to reject the H0
