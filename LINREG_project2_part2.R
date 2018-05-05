###########
### 2.a ###
###########


# Plot the number of cars against the time of day. Which variable would you expect to influence the
# PM10 concentration, time of day or number of cars?
pm10 = read.delim("pm10.txt")
pm10$highpm10 = factor(pm10$highpm10, levels=c(0,1),
                       labels=c("no","yes"))
pm10$time = factor(pm10$time, levels=c(3,1,2,4),
                   labels=c("12-18","1-6","6-12","18-24"))

# table(pm10$highpm10[which(pm10$time == "1-6")])
# 10/(120)

# plot cars against time of day:
plot(pm10$time, pm10$cars, xaxt = "n", ylab="number of cars", xlab="time of day")
axis(1, at=c(1,2,3,4), labels=c("1-6","6-12","12-18","18-24"))


###########
### 2.b ###
###########

# Fit a model where the probability of PM10 exceeding 50 ??g/m3 depends on the number of cars. 
# Report the odds ratio for cars and its 95 % confidence intervals. How much does the odds 
# change if the number of cars is increased from 1000 to 1100?

model2 = glm(highpm10 ~ cars, data=pm10, family="binomial")
summary(model2)

# odds ratio
OR.1 = exp(coef(model2))
# ci:
exp(confint(model2))
# ci for change of 100:
OR.1^100


###########
### 2.c ###
###########

# Use the model from (b) and estimate the probability of PM10 exceeding 50 ??g/m3 when the 
# number of cars is 300 and when it is 3000. Also calculate 95 % confidence intervals for 
# the probabilities.

# computing the probabilities:
predx300 = data.frame(cars = 300)
predx3000 = data.frame(cars = 3000)

p.hat300 = predict(model2, predx300, type="response")
p.hat3000 = predict(model2, predx3000, type="response")


# creating confidence intervals for the probabilities:
# log(Odds):
logit.hat300 = predict(model2 , predx300 , se.fit=TRUE) 
logit.hat3000 = predict(model2 , predx3000 , se.fit=TRUE) 

# Obtain ci for logodds:
logit.lo300 = logit.hat300 $fit - qnorm(0.975)*logit.hat300 $se.fit
logit.hi300 = logit.hat300 $fit + qnorm(0.975)*logit.hat300 $se.fit
logit.lo3000 = logit.hat3000 $fit - qnorm(0.975)*logit.hat3000 $se.fit
logit.hi3000= logit.hat3000 $fit + qnorm(0.975)*logit.hat3000 $se.fit

# exponentiate to obtain CI for odds:
ci.odds300 <- cbind(lo = exp(logit.lo300 ), hi=exp(logit.hi300 ))
ci.odds3000 <- cbind(lo = exp(logit.lo3000 ), hi=exp(logit.hi3000 ))

# Obtain ci for probability from ci of odds:
ci.p300 <- ci.odds300 /(1+ci.odds300 )
ci.p3000 <- ci.odds3000 /(1+ci.odds3000 )


###########
### 2.d ###
###########

# We might suspect that it would be better to use the logarithm of the number of cars instead. Fit a
# model where the probability of PM10 exceeding 50 ??g/m3 depends on the logarithm of number of
# cars. Report the odds ratio for log cars and its 95 % confidence interval. How much does the odds
# change if the number of cars is increased from 1000 to 1100?

model3 = glm(highpm10 ~ log(cars), data=pm10, family="binomial") 
summary(model3)

exp(coef(model3)[2]) #logratio for beta_log(cars)

exp(confint(model3)[2,])

predx = data.frame(cars = 1100)
logit.hat = predict(model3 , predx , se.fit=TRUE) 

# we get for cars=1000 that logodds = -4.341586 
# we get for cars=1100 that logodds = -1.335786, so the logodds increases by 3.0058
# which means that the odds increases by (exp(beta_logcars))^log(1100/1000) = 1.058128 

# Constructing confidence interval for the logodds(e^beta_logcars):
exp(confint(model3)[2,])


###########
### 2.e ###
###########

# Use the model from (d) to estimate the probability of PM10 exceeding 50 ??g/m3 when the number
# of cars is 300 and when it is 3000. Also calculate 95 % confidence intervals for the probabilities.
# Compare with the result in (c). Any major differences?
  
# computing the probabilities:
predx300 = data.frame(cars = 300)
predx3000 = data.frame(cars = 3000)

# estimated probabilities:
p.hat300 = predict(model3, predx300, type="response")
p.hat3000 = predict(model3, predx3000, type="response")


# creating confidence intervals for the probabilities:
# log(Odds):
logit.hat300 = predict(model3 , predx300 , se.fit=TRUE) 
logit.hat3000 = predict(model3 , predx3000 , se.fit=TRUE) 

# Obtain ci for logodds:
logit.lo300 = logit.hat300 $fit - qnorm(0.975)*logit.hat300 $se.fit
logit.hi300 = logit.hat300 $fit + qnorm(0.975)*logit.hat300 $se.fit
logit.lo3000 = logit.hat3000 $fit - qnorm(0.975)*logit.hat3000 $se.fit
logit.hi3000= logit.hat3000 $fit + qnorm(0.975)*logit.hat3000 $se.fit

# exponentiate to obtain CI for odds:
ci.odds300 <- cbind(lo = exp(logit.lo300 ), hi=exp(logit.hi300 ))
ci.odds3000 <- cbind(lo = exp(logit.lo3000 ), hi=exp(logit.hi3000 ))

# Obtain ci for probability from ci of odds:
ci.p300 <- ci.odds300 /(1+ci.odds300 )
ci.p3000 <- ci.odds3000 /(1+ci.odds3000 )



###########
### 2.f ###
###########

pm10 = read.delim("pm10.txt")

plot(pm10$cars, pm10$highpm10)
lines(ksmooth(pm10$cars, pm10$highpm10, bandwidth = 1500))

AIC(model2)
AIC(model3)
AIC(model2, k=log(nrow(pm10)))
AIC(model3, k=log(nrow(pm10)))

summary(model2)
summary(model3)



###########
### 2.g ###
###########

# We also would like to plot the kernel smoothing against the predictions of the two models
# creating confidence intervals for the probabilities:
predx = data.frame(cars = seq(1,4000,1))


# log(Odds):
logit.hat2 = predict(model2 , predx , se.fit=TRUE) 
logit.hat3 = predict(model3 , predx , se.fit=TRUE) 

# Obtain ci for logodds:
logit.lo2 = logit.hat2 $fit - qnorm(0.975)*logit.hat2 $se.fit
logit.hi2 = logit.hat2 $fit + qnorm(0.975)*logit.hat2 $se.fit
logit.lo3 = logit.hat3 $fit - qnorm(0.975)*logit.hat3 $se.fit
logit.hi3 = logit.hat3 $fit + qnorm(0.975)*logit.hat3 $se.fit

# exponentiate to obtain CI for odds:
ci.odds2 <- cbind(lo = exp(logit.lo2 ), hi=exp(logit.hi2 ))
ci.odds3 <- cbind(lo = exp(logit.lo3 ), hi=exp(logit.hi3 ))

# Obtain ci for probability from ci of odds:
ci.p2 <- ci.odds2 /(1+ci.odds2 )
ci.p3 <- ci.odds3 /(1+ci.odds3 )


plot(predict(model2,predx, type="response"), pch='.', col="green")
lines(ksmooth(pm10$cars, pm10$highpm10, bandwidth = 2000))
lines(seq(1,4000,1), ci.p2[,"lo"],lty=2,col="red",lwd=2)
lines(seq(1,4000,1), ci.p2[,"hi"],lty=2,col="red",lwd=2)


predx = data.frame(cars = seq(1,4000,1))
plot(predict(model3,predx, type="response"), pch='.', col="green")
lines(ksmooth(pm10$cars, pm10$highpm10, bandwidth = 200))
lines(seq(1,4000,1), ci.p3[,"lo"],lty=2,col="red",lwd=2)
lines(seq(1,4000,1), ci.p3[,"hi"],lty=2,col="red",lwd=2)
abline(v=257.2177)

plot(pm10$cars, pm10$highpm10)


###########
### 2.h ###
###########

# If we want the probability of PM10 exceeding 50 ??g/m3
# to be less than 10 %, how many cars per hour
# we can allow, according to model (d)?

# We simply have to compute the corresponding log(odds), and use the model equation to solve for x.

p = 0.1
odds = 0.1/(1-0.1)
beta0 = coef(model3)[1]
beta1 = coef(model3)[2]

x = odds^(1/beta1)*exp(-beta0/beta1)



###########
### 2.i ###
###########

leverage = influence(model3)$hat
plot(leverage^2)
pearson.res = influence(model3)$pear.res
plot(pearson.res)  
deviance.contribution = influence(model3)$dev.res
plot(deviance.contribution)
cooks = cooks.distance(model3)
plot(cooks)
which(cooks>0.06)
which(cooks> 0.03)
which(pm10$cars <=74)
outliers = c(148, 204, 250, 297, 339, 373, 424, 476)
check = 204
pm10$highpm10[check]
pm10$cars[outliers]
pm10$temp2m[outliers] 
pm10$highpm10[outliers]
pm10$time[outliers]
pm10$winddirection[outliers]
pm10$windspeed[outliers]
plot(pm10$windspeed)
points(outliers,pm10$windspeed[outliers],col=c("red"),pch=19)
table(pm10$winddirection)
