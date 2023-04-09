# ACTUAL ASSIGNMENT

#1) population of 10000 with mean 4 and standard deviation 5
populationVector <- rnorm(10000, 4, 5)

# make variables out of size mean and standard deviation
N <- length(populationVector)
mean <- mean(populationVector)
sd <- sd(populationVector)

#make a qqplot and a histogram with normal density curve for populationVector
qqnorm(populationVector)
qqline(populationVector)
hist(populationVector, freq = FALSE)
xfit <- seq(min(populationVector), max(populationVector), length = 40) 
yfit <- dnorm(xfit, mean = mean(populationVector), sd = sd(populationVector))
lines(xfit, yfit)

# make a variable from an exponential distribution
exponentialDistVariable <- rexp(N, 1)

# make variables out of size mean and standard deviation
XN <- length(exponentialDistVariable)
Xmean <- mean(exponentialDistVariable)
Xsd <- sd(exponentialDistVariable)

#make a qqplot and a histogram with normal density curve for populationVector
qqnorm(exponentialDistVariable)
qqline(exponentialDistVariable)
hist(exponentialDistVariable, freq = FALSE)
xfit <- seq(min(exponentialDistVariable), max(exponentialDistVariable), length = 40) 
yfit <- dnorm(xfit, mean = mean(exponentialDistVariable), sd = sd(exponentialDistVariable))
lines(xfit, yfit)

#2) GRAPHS FOR POPULATIONVECTOR

# make a random sample of size 10 of populationVector

sample1 <- sample(populationVector, 10)

#make a qqplot and a histogram with normal density curve for sample1
qqnorm(sample1)
qqline(sample1)
hist(sample1, freq = FALSE)
xfit <- seq(min(sample1), max(sample1), length = 40) 
yfit <- dnorm(xfit, mean = mean(sample1), sd = sd(sample1))
lines(xfit, yfit)

# make a random sample of size 50 of populationVector

sample2 <- sample(populationVector, 50)

#make a qqplot and a histogram with normal density curve for sample2
qqnorm(sample2)
qqline(sample2)
hist(sample2, freq = FALSE)
xfit <- seq(min(sample2), max(sample2), length = 40) 
yfit <- dnorm(xfit, mean = mean(sample2), sd = sd(sample2))
lines(xfit, yfit)

# make a random sample of size 500 of populationVector

sample3 <- sample(populationVector, 500)


#make a qqplot and a histogram with normal density curve for sample3
qqnorm(sample3)
qqline(sample3)
hist(sample3, freq = FALSE)
xfit <- seq(min(sample3), max(sample3), length = 40) 
yfit <- dnorm(xfit, mean = mean(sample3), sd = sd(sample3))
lines(xfit, yfit)

#GRAPHS FOR EXPONENTIALDISTVARIABLE


Xsample1 <- sample(exponentialDistVariable, 10)

#make a qqplot and a histogram with normal density curve for Xsample1
qqnorm(Xsample1)
qqline(Xsample1)
hist(Xsample1, freq = FALSE)
xfit <- seq(min(Xsample1), max(Xsample1), length = 40) 
yfit <- dnorm(xfit, mean = mean(Xsample1), sd = sd(Xsample1))
lines(xfit, yfit)

# make a random sample of size 50 of exponentialDistVariable

Xsample2 <- sample(exponentialDistVariable, 50)

#make a qqplot and a histogram with normal density curve for Xsample2
qqnorm(Xsample2)
qqline(Xsample2)
hist(Xsample2, freq = FALSE)
xfit <- seq(min(Xsample2), max(Xsample2), length = 40) 
yfit <- dnorm(xfit, mean = mean(Xsample2), sd = sd(Xsample2))
lines(xfit, yfit)

# make a random sample of size 500 of exponentialDistVariable

Xsample3 <- sample(exponentialDistVariable, 500)


#make a qqplot and a histogram with normal density curve for sample3
qqnorm(Xsample3)
qqline(Xsample3)
hist(Xsample3, freq = FALSE)
xfit <- seq(min(Xsample3), max(Xsample3), length = 40) 
yfit <- dnorm(xfit, mean = mean(Xsample3), sd = sd(Xsample3))
lines(xfit, yfit)

# 3) Making confidence intervals of size 50 for populationVector distribution

# samples 50 from the population vector
sample50 <- sample(populationVector, 50)

# record the size, mean and sd of sample3

n50 <- length(sample50)
mean50 <- mean(sample50)
sd50 <- sd(sample50)

# calculate z score for 95%
Z_score <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x1
t_score <- qt(0.975, n50-1)

#calculate the standardn error of mean50 using known and estimated sd50
se_kn <- sd50/sqrt(n50)
se_unkn <- sd50/sqrt(n50)


# a) calculate the CI for the sample using the Z-score and the lnown sd

left_Z95_kn <- mean50-Z_score*se_kn
right_z95_kn <- mean50+Z_score*se_kn

# b) Calculate the CI for the sample using the T-dist and the sample SD

left_t95_unkn <- mean50-t_score*se_unkn
right_t95_unkn <- mean50+t_score*se_unkn

#Confidence intervals for exponential distribution

# samples 50 from the population vector
Xsample50 <- sample(exponentialDistVariable, 50)

# record the size, mean and sd of sample3

Xn50 <- length(Xsample50)
Xmean50 <- mean(Xsample50)
Xsd50 <- sd(Xsample50)

# calculate z score for 95%
XZ_score <- qnorm(0.975)

#Calculate critical t-value for 95% CI and x1
Xt_score <- qt(0.975, Xn50-1)

#calculate the standardn error of mean50 using known and estimated sd50
Xse_kn <- Xsd50/sqrt(Xn50)
Xse_unkn <- Xsd50/sqrt(Xn50)


# a) calculate the CI for the sample using the Z-score and the lnown sd

Xleft_Z95_kn <- Xmean50-XZ_score*Xse_kn
Xright_z95_kn <- Xmean50+XZ_score*Xse_kn

# b) Calculate the CI for the sample using the T-dist and the sample SD

Xleft_t95_unkn <- Xmean50-Xt_score*Xse_unkn
Xright_t95_unkn <- Xmean50+Xt_score*Xse_unkn

#show results
paste("var: populationVector", "mean:", mean, "sd:", sd)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", left_Z95_kn, right_z95_kn)
paste("t-distribution w/unknown pop sd:", left_t95_unkn, right_t95_unkn)
paste("var: exponentialDistVariable", "mean:", Xmean, "sd:", Xsd)
paste("95% CIs for estimate of population mean")
paste("Z-distribution w/known pop sd:", Xleft_Z95_kn, Xright_z95_kn)
paste("t-distribution w/unknown pop sd:", Xleft_t95_unkn, Xright_t95_unkn)