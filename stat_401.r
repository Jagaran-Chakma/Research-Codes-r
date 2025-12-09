#1. Consider the random sample: X = {3.56, 0.69, 0.10, 1.84, 3.93, 1.25, 0.18, 1.13, 
# 0.27, 0.50, 0.67, 0.01, 0.61, 0.82, 1.70, 0.39, 0.11, 1.20, 1.21, 0.72}. Perform a 
#bootstrap analysis with 10,000 re-samples and: 

x <- c(3.56, 0.69, 0.10, 1.84, 3.93, 1.25, 0.18, 1.13, 0.27, 0.50,
       0.67, 0.01, 0.61, 0.82, 1.70, 0.39, 0.11, 1.20, 1.21, 0.72)

n<-length(x);n


#(a) Obtain the bootstrap distribution of sample mean and sample standard deviation. 
B<-10000
boot_mean<-numeric(B)
boot_sd<-numeric(B)

set.seed(123)
for(i in 1:B){
  sample_i<-sample(x,n,replace = TRUE) #vector,sample size, replace
  boot_mean[i]<-mean(sample_i)
  boot_sd[i]<-sd(sample_i)
}
head(boot_mean)
boot_sd

hist(boot_mean, xlab ="Bootstap Mean", col="green")
hist(boot_sd, main = "Distribution of Standard Deviation",xlab ="sd",col = "blue")



#(b)Construct 95% confidence intervals for both statistics using the percentile method.
CI_mean<-quantile(boot_mean, probs = c(.025,0.975)); CI_mean
CI_sd<-quantile(boot_sd, probs = c(.025,0.975)); CI_sd



#(c) Compare the bootstrap estimates with the original sample estimates. 
org_mean<-mean(x);org_mean
org_sd<-sd(x);org_sd

avr_boot_mean<-mean(boot_mean);avr_boot_mean
avr_boot_sd<-mean(boot_sd);avr_boot_sd




#2.Using the same data set from Problem 1, perform a Jackknife analysis for the sample standard deviation: 
#(a)Compute the Jackknife replications ˆσ(i) by omitting each observation in turn.
x= c(3.56, 0.69, 0.10, 1.84, 3.93, 1.25, 0.18, 1.13, 0.27, 0.50,
     0.67, 0.01, 0.61, 0.82, 1.70, 0.39, 0.11, 1.20, 1.21, 0.72)

n <- length(x)
jack_s <- numeric(n)

for (i in 1:n){
  xi <- x[-i]         # leave one out each observation
  jack_s[i] <- sd(xi)  # sd of the reduced sample
}

jack_s

jack_mean <- mean(jack_s)
jack_mean


#(b)Compute the corresponding pseudo-values P Vi.  
s_full <- sd(x)
pseudo_values<- n*s_full -(n-1)*jack_s
pseudo_values

#(c)Compute the Jackknife estimate ˆσJack as the average of pseudo-values.
sigma_jack <- mean(pseudo_values)
sigma_jack

#(d)Determine the bias of the original sample standard deviation.
bias_jack <- (n-1)*(jack_mean - s_full)
bias_jack



#3. Suppose a sample is drawn from N(µ, σ2) with known variance σ2 = 9. Using the 
# Sequential Probability Ratio Test (SPRT), test:  
#   H0 : µ = 9 vs H1 : µ = 15  
# at significance level α = 0.05 and type-II error β = 0.01.  

mu0 <- 9           # H0 mean
mu1 <- 15          # H1 mean
sigma2 <- 9        # known variance
sigma <- sqrt(sigma2)
alpha <- 0.05
beta  <- 0.01

# (a) Likelihood ratio and log-LR
# Likelihood ratio for a single observation
LR_single <- function(x) {
  num = dnorm(x, mean = mu1, sd = sigma)
  den = dnorm(x, mean = mu0, sd = sigma)
  num / den
}

# log-likelihood ratio for a single observation
logLR_single <- function(x) {
  log(LR_single(x))
}

# For n observations: cumulative log-likelihood ratio
logLR_n <- function(xvec) {
  sum(logLR_single(xvec))
}

# (b) Determine the decision boundaries and the stopping rule for the SPRT.  
A <- (1 - beta) / alpha         # Upper boundary ratio
B <- beta / (1 - alpha)         # Lower boundary ratio
logA <- log(A)
logB <- log(B)
A; B
logA; logB



# Boundaries in terms of sum S_n and mean Xbar_n
# For N(µ, σ²), the log-likelihood ratio simplifies to:
# logLR = (2/3)*sum(x - 12)

# Constants for boundaries:
cA <- (3/2) * logA
cB <- (3/2) * logB
cA; cB

# (c) Full SPRT stopping rule
SPRT <- function(data) {
  
  Sn <- 0          # cumulative sum
  n  <- 0          # sample size
  
  for (x in data) {
    n <- n + 1
    Sn <- Sn + x
    
    logLR <- (2/3) * (Sn - 12 * n)
    
    if (logLR >= logA) {
      return(list(decision = "Reject H0 (Accept H1)",
                  n = n, logLR = logLR))
    }
    
    if (logLR <= logB) {
      return(list(decision = "Accept H0",
                  n = n, logLR = logLR))
    }
  }
  return(list(decision = "No decision — continue sampling",
              n = n,
              logLR = logLR))
}

# Example run (you can replace with real data)
set.seed(123)
example_data <- rnorm(500, mean = 15, sd = sigma)  # sample from H1
SPRT(example_data)

