pm10 = read.delim("pm10.txt")

###########
### 1.a ###
###########

# here we want to estimate the probability that the particle concentration on a randomly chosen
# hour exceeds 50 mikrograms/m^3. We take the mean of the first column to do this.

p.est = mean(pm10[,1])
n = length(pm10[,1])

# We estimate that this probability is 0.228. Since n is quite large 500, we can create a confidence
# interval in the usual way by using a normalapproximation. Since Y (exceed or not exceed)is 
# bernoulli distributed, the standard error of the estimate (of p) is approximated by 
# hat(p)(1-hat(p))/n. 

std = sqrt(p.est*(1-p.est)/n)
UB = p.est + qnorm(0.975)*std
LB = p.est - qnorm(0.975)*std
CI = c(LB,UB)

# We get that an approximate 95% CI for p (prob of exceedence of avg. air pollution on any given 
# hour) is (0.1912261 0.2647739). Due to the large sample size, this CI is quite tight. It is 
# important to keep in mind model assumptions: We assume that this probability remains constant
# over time. This of course is probably not exactly true, as conditions tends to change with time,
# but it may hold approximately if we are dealing with sufficiently short time intervals.

###########
### 1.b ###
###########

pm10$highpm10 = factor(pm10$highpm10, levels=c(0,1),
                     labels=c("no","yes"))
pm10$time = factor(pm10$time, levels=c(3,1,2,4),
                         labels=c("12-18","1-6","6-12","18-24"))

# we use time 3 as default level since we have the most observations from this category.
table(pm10$time)

model1 = glm(highpm10 ~ time, data=pm10, family="binomial")
summary(model)
or1 = exp(model$coefficients) # exponentiate the coefficients to get the odds RATIOS
ODDS = c(exp.coef[1], exp.coef[1]*exp.coef[2], exp.coef[1]*exp.coef[3], exp.coef[1]*exp.coef[4])


# confidence intervals of the odds ratios:
exp(confint(model1))



###########
### 1.c ###
###########

# Use the model to estimate the probability of PM10 exceeding 50 ??g/m3
# at 3 o'clock in the morning,
# at 11 o'clock in the morning, at 15 o'clock in the afternoon and at 23 o'clock in the evening. Also
# calculate 95 % confidence intervals for the probabilities.

predx_3 = data.frame(time = "1-6")
predx_11 = data.frame(time = "6-12")
predx_15 = data.frame(time = "12-18")
predx_23 = data.frame(time = "18-24")

p.hat3 = predict(model1, predx_3, type="response")
p.hat11 = predict(model1, predx_11, type="response")
p.hat15 = predict(model1, predx_15, type="response")
p.hat23 = predict(model1, predx_23, type="response")


# Here we do confidence intervals for the probabilities. First we make confidence intervals for
# the log odds:

T = c(predx_3,predx_11,predx_15,predx_23)
cis.p = matrix(0,4,3)
for (i in 1:4){
  p.hat = predict(model1, T[i], type="response")
  
  # log(Odds):
  logit.hat = predict(model1 , T[i] , se.fit=TRUE) 
  # Obtain ci for logodds:
  logit.lo = logit.hat $fit - qnorm(0.975)*logit.hat $se.fit
  logit.hi= logit.hat $fit + qnorm(0.975)*logit.hat $se.fit
  # exponentiate to obtain CI for odds:
  ci.odds <- cbind(lo = exp(logit.lo ), hi=exp(logit.hi ))
  # Obtain ci for probability from ci of odds:
  ci.p <- ci.odds /(1+ci.odds )
  
  # collect ci's in matrix
  cis.p[i,] = c(ci.p, p.hat) 
}

# plot of estimated probabilities with confidence intervals:

plot(c(3,11,15,23), cis.p[,3],ylim=c(0, 0.6), xlab = "hello", ylab="probability")
legend("topright", legend=c("confidence intervals", "estimated probability"),
       col=c("red", "black"), cex=0.8, pch = c(1,1))
points(c(3,11,15,23), cis.p[,1], col="red")
points(c(3,11,15,23), cis.p[,2], col="red")








