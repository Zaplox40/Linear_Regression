# Normal(mu, sigma^2)
N = 10000000
mu = 5
sigma = 5
# randomly generate N samples from the underlying population
X = rnorm(N, mean = mu, sd = sigma)

#Expectations
E = mu 
Var = sigma^2
sd = sigma

# Plot the histogram of N samples 
# randomly draw from the underlying population distribution Normal(mu, sigma^2)

hist(X, 
     col = "blue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
#Draws from Underlying Distribution
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Histogram","Underlying Distribution"),fill=c("blue","red"),cex = 0.5 )

# The estimator is the sample mean: 1\n sum x_i

# The following is the sample size vector contains 24 samples
Sample_Size = 2^(0:23)
Sample_Mean = numeric(length = length(Sample_Size))
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(mu-sigma,mu+sigma), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'blue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
abline(a = NULL, b = NULL, h = mu, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("blue","red"),cex = 0.5 )

# Binomial Distribution

N = 10000000
n_trials = 60
p = 0.2

# Randomly generate N samples from the underlying population
X = rbinom(n=N, size = n_trials, prob = p)

#Theoretical Expectations
E = n_trials*p 
Var = n_trials*p*(1-p) 
sd = as.integer(sqrt(Var))

#Plot the histogram of N Samples
#Randomly draw from the underlying population distribution N(mu,sigma^2)

hist(X,
     col = "blue",
     prob = FALSE,
     breaks = seq(0, n_trials,1),
     main = 'Sample Histogram and Underlying Distribution', cex.main=0.75)

x = seq(0,n_trials,1)
lines(x,dbinom(x,n_trials,p)* N ,col="red")
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#Underlying Population which is n_Trials*p=50*0.4=20. 
#The estimator is the sample mean: 1\n sum x_i

#The following is the sample size vector contains 24 samples
Sample_Size = 2^(0:23)
# Initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# Calculate the sample mean for each sample size 
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )


# t-distribution(df)
N = 10000000
df = 10
X = rt(N, df, ncp = 0)
E = 0
Var = ifelse(df > 2, df/(df-2), Inf)
sd = sqrt(Var)

# draw the histogram of N samples randomly draw 
# from the underlying population distribution t(df)
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlim= c(-10,10), 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution t(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dt(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Underlying population mu. The estimator is the sample mean: 1\n sum x_i

# the following is the sample size vector contains 24 samples
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'blue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

#f-distribution(df1,df2)

N = 10000000
df1 = 9
df2 = 7
# randomly generate N samples from the underlying population
X = rf(N, df1, df2)
E = df2/(df2-2)
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4))
sd = sqrt(Var) 

# draw the histogram of N samples randomly draw 
hist(X, 
     col = "brown" , 
     freq = FALSE, 
     breaks = 5000, 
     xlim= c(0,20),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# Draw the the underlying population distribution f(df1, df2)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(df(x,df1,df2), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#The estimator is the sample mean: 1\n sum x_i

# the following is the sample size vector contains 24 samples
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
# plot the sample mean for each sample size candidiate
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = "brown",
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("brown","red"),cex = 0.5 )

#Chi distribution

N = 10000000
df = 7
# randomly generate N samples from the underlying population
X = rchisq(N, df, ncp = 0)
E = df
Var =2*df
sd = as.integer(sqrt(Var))

# draw the histogram of N samples randomly draw from the underlying population distribution Chi(df)
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Chi(df)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dchisq(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Underlying population mu. The estimator is the sample mean: 1\n sum x_i
# the sample size vector contains 24 sample sieze
Sample_Size = 2^(0:23)
# initialize a vector with 24 entries to store the sample mean value with each sample size candidiate
Sample_Mean = numeric(length = length(Sample_Size))
# calculate the sample mean for each sample size candidiate
for (i in 1:length(Sample_Size)) {
  Sample_X = sample(X, size = Sample_Size[i] , replace = FALSE, prob = NULL)
  Mean_X = mean(Sample_X)
  Sample_Mean[i]=Mean_X
}
plot(Sample_Size, Sample_Mean, log = "x", ylim =c(E-sd,E+sd), 
     xlab ='Sample Size', ylab = 'Sample Mean', col = 'steelblue',
     main = 'Sample Mean Converge to Population Mean',cex.main=0.75)
# Plot the Theoretical Expectation or Population Mean
abline(a = NULL, b = NULL, h = E, col = 'red')
legend("topright",c("Sample Mean","Population Mean"),fill=c("steelblue","red"),cex = 0.5 )

#Normal Distribution(mu, sigma^2)
N = 10000000
mu = 20
sigma = 5
X = rnorm(N, mean = mu, sd = sigma)
E = mu
Var = sigma^2
sd = sigma

#Histogram

hist(X, 
     col = "blue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dnorm(x, mean = mu, sd = sigma), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#small Sample and Large Sample
Sample_Size = c(10, 100000 )
reps = 2000
#i=1 #small sample 
#i=2 #large sample
i=1
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# Plot Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#Binomial DIstribution
N = 10000000
n_Trials = 40
p = 0.2
# randomly generate N samples from the underlying population
X = rbinom(n=N, size = n_Trials, prob=p)

E = n_Trials*p 
Var = n_Trials*p*(1-p) 
sd = as.integer(sqrt(Var)) 
sd = sqrt(Var) 
#Draw the histogram of N samples randomly draw from the underlying population distribution Normal(mu, sigma^2)
hist(X, 
     col = "steelblue" , 
     prob = FALSE,
     breaks = seq(0,n_Trials,1),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Binomial(n,p)
x = seq(0,n_Trials,1)
lines(x,dbinom(x,n_Trials,p)* N ,col="red")
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#small Sample vs Large Sample
Sample_Size = c(2, 100000 )
#number of repetition
reps = 2000
#i=1 #small sample 
#i=2 #large sample
i=2
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
# calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(0,n_Trials,1)
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# t-distribution(df)
N = 10000000
df = 10
X = rt(N, df, ncp = 0)
E = 0
Var = ifelse(df > 2, df/(df-2), Inf)
sd = sqrt(Var)
# Histogram
hist(X, 
     col = "blue" , 
     freq = FALSE, 
     breaks = 200, 
     xlim= c(-10,10), 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)

x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(dt(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )


# small Sample vs Large Sample
Sample_Size = c(2, 100000 )
# number of repetition
reps = 2000
### 
#i=1 #small sample 
#i=2 #large sample
i=2
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
# calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# Plot the Normalized Sample Mean together with Standard Normal Distribution Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
# Plot the density histogram for Sample Means of reps many of repetitions
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

# F-Distribution
N = 10000000
df1 = 9
df2 = 7 
X = rf(N, df1, df2)
E = df2/(df2-2)
Var =2*df2^2*(df1+df2-2)/(df1*(df2-2)^2*(df2-4)) 
sd = sqrt(Var)

#Histogram
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 5000, 
     xlim= c(0,20),
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
#Draw f(df1, df2)
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
curve(df(x,df1,df2), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

Sample_Size = c(2, 100000 )
reps = 2000
#i=1 #small sample 
#i=2 #large sample
i=1
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#Plot the Normalized Sample Mean together with Standard Normal Distribution
#Normalize Sample mean
Nsample_Means = (sample_Means-E)/(sd/sqrt(Sample_Size[i]))
hist(Nsample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     xlab = 'Normalized Sample Mean',
     main = 'Normalized Sample Mean Histogram and Standard Normal Distribution',cex.main=0.75)
# draw the standard Normal Distribution
curve(dnorm(x, mean = 0, sd = 1), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#Chi(df)
N = 10000000
df = 5

X = rchisq(N, df, ncp = 0)
E = df
Var =2*df 
sd = sqrt(Var)

#Histogram for N 
x = seq(range(X)[1], range(X)[2], by = diff(range(X))/50 )
hist(X, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 50, 
     main = 'Sample Histogram and Underlying Distribution',cex.main=0.75)
# draw the the underlying population distribution Chi(df)
curve(dchisq(x,df), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topright",c("Sample Histogram","Underlying Distribution"),fill=c("steelblue","red"),cex = 0.5 )

#small Sample vs Large Sample
Sample_Size = c(2, 100000 )
reps = 2000
#i=1 #small sample 
#i=2 #large sample
i=2
samples <- replicate(reps, sample(X,size = Sample_Size[i],replace = FALSE, prob = NULL) )
## calculate the sample mean for each iteration
sample_Means <- colMeans(samples)

# Plot the density histogram for Sample Means of reps many of repetitions
hist(sample_Means, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 200, 
     main = 'Sample Mean Histogram and Normal Distribution',cex.main=0.75)
# draw the convergent Normal(E, Var)
x = seq(range(sample_Means)[1], range(sample_Means)[2], by = diff(range(sample_Means))/50 )
curve(dnorm(x, mean = E, sd = sd/sqrt(Sample_Size[i])), 
      col = "red", 
      lwd = "2", 
      add = TRUE)
legend("topleft",c("Sample Mean Histogram","Normal Distribution"),fill=c("steelblue","red"),cex = 0.5 )