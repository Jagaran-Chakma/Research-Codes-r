#1The following data gives remission times for two groups of acute leukemia patients. One 
# groups given the placebo and the other the drug 6-MP. (+ indicates censoring time) 
# Actual 6 6 6 6+ 7 9+ 10 10+ 11 13 16 17+ 18 19 19+ 20+ 22 23 25+ 32+ 32+ 34+ 35+ 37+
# Placebo 1 1 2 2 3 4 4 5 5 8 8 8 8 9 8 11 11 12 12 15 17 22 23 25 

library(survival)
library(survminer)

# Actual group
time_actual <- c(6,6,6,6,7,9,10,10,11,13,16,17,18,19,19,20,22,23,25,32,32,34,35,37)
status_actual <- c(1,1,1,0,1,0,1,0,1,1,1,0,1,1,0,0,1,1,0,0,0,0,0,0)

# Placebo group
time_placebo <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,9,8,11,11,12,12,15,17,22,23,25)
status_placebo <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

# Combine data
group <- c(rep("Actual", length(time_actual)),
           rep("Placebo", length(time_placebo)))
time <- c(time_actual, time_placebo)
status <- c(status_actual, status_placebo)
dat1 <- data.frame(time, status, group)


#(a)Calculate the product limit estimator of the survivor functions for two groups. 
fit_km <- survfit(Surv(time, status) ~ group, data = dat1)
summary(fit_km)


#(b)Plot the estimated survivor function for the two groups. 
ggsurvplot(fit_km, data = dat1,conf.int = TRUE,risk.table = TRUE,xlab = "Time",
           ylab = "Survival Probability",legend.title = "Group")


#(c)Also calculate the cumulative hazard function estimator plot then and comment. 
ggsurvplot(fit_km, fun = "cumhaz",data = dat1,
           xlab = "Time",ylab = "Cumulative Hazard",legend.title = "Group")




#2 Using the following survival dataset of 30 patients where each observation includes a survival 
# time, event status (1 = death, 0 = censored), and treatment group. 

# ID Time Status Group ID Time Status Group 
# 1 3 1 Drug-1 16 2 1 Drug-2 
# 2 5 1 Drug-1 17 4 1 Drug-2 
# 3 6 0 Drug-1 18 5 0 Drug-2 
# 4 7 1 Drug-1 19 6 1 Drug-2 
# 5 9 1 Drug-1 20 8 1 Drug-2 
# 6 10 0 Drug-1 21 9 1 Drug-2 
# 7 12 1 Drug-1 22 10 0 Drug-2 
# 8 14 0 Drug-1 23 11 1 Drug-2 
# 9 15 1 Drug-1 24 13 1 Drug-2 
# 10 18 0 Drug-1 25 16 0 Drug-2 
# 11 19 1 Drug-1 26 17 0 Drug-2 
# 12 21 0 Drug-1 27 20 1 Drug-2 
# 13 24 1 Drug-1 28 22 1 Drug-2 
# 14 25 1 Drug-1 29 23 1 Drug-2 
# 15 27 0 Drug-1 30 28 0 Drug-2 


#a)Write the R code required to perform the log-rank test. Also report the chi-square 
#statistic, degrees of freedom, and p-value. 
library(survminer)
ID <- 1:30
Time <- c(3,5,6,7,9,10,12,14,15,18,19,21,24,25,27,
          2,4,5,6,8,9,10,11,13,16,17,20,22,23,28)
Status <- c(1,1,0,1,1,0,1,0,1,0,1,0,1,1,0,
            1,1,0,1,1,1,0,1,1,0,0,1,1,1,0)
Group <- c(rep("Drug-1", 15), rep("Drug-2", 15))
dat2 <- data.frame(ID, Time, Status, Group)
logrank <- survdiff(Surv(Time, Status) ~ Group, data = dat2)
logrank


chisq  <- logrank$chisq
df     <- length(logrank$n) - 1
pvalue <- 1 - pchisq(chisq, df)

chisq; df; pvalue


#b()
# p≈0.51>0.05,
# we fail to reject the null hypothesis.
# There is no statistically significant difference in survival between Drug-1
# and Drug-2 at the 5% significance level.




#c)Interpret the result in the context of the treatments.
# The log-rank test indicates that the time-to-event (survival) outcomes for patients receiving Drug-1 and 
# Drug-2 are not significantly different.




#3  
# (a) Find the Cox-Proportional Hazard model ℎ(𝑡)=ℎ0(𝑡) exp (𝛽1Group+𝛽2 Age). 
# (b) Describe how you would test the proportional hazards (PH) assumption using R. 
# (c) Comment on your findings.

ID <- 1:26
Time <- c(3,6,8,9,12,14,16,18,20,22,23,24,25,
          2,5,7,8,9,11,13,14,15,17,18,20,22)

Status <- c(1,1,0,1,1,0,1,0,1,0,1,1,0,
            1,1,0,1,1,0,1,1,1,0,1,1,0)

Group <- c(rep("G-1", 13), rep("G-2", 13))

Age <- c(45,50,47,52,60,55,49,46,57,53,54,55,57,
         46,47,47,48,50,62,59,55,51,58,60,63,64)

dat3 <- data.frame(ID, Time, Status, Group, Age)

#(a) Fit Cox PH model
coxfit <- coxph(Surv(Time, Status) ~ Group + Age, data = dat3)
summary(coxfit)

#(b) Test Proportional Hazards (PH) assumption
ph_test <- cox.zph(coxfit)
ph_test
plot(ph_test)


# c)comment based on findings
# For Group, the trend shows slight curvature, consistent with its borderline p-value (0.051), suggesting a possible but weak time-dependent effect.
# For Age, the line appears flat with no clear pattern, consistent with its non-significant test result.
# The global PH test (p = 0.123) confirms that the model as a whole satisfies the proportional hazards assumption.


