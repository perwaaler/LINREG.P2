install.packages(pscl)
###########
### 3.a ###
###########

# Starting with model (d), use a suitable test to determine whether adding the time of day would improve
# the model.

pm10 = read.delim("pm10.txt")

pm10$highpm10 = factor(pm10$highpm10, levels=c(0,1),
                       labels=c("no","yes"))
pm10$time = factor(pm10$time, levels=c(3,1,2,4),
                   labels=c("12-18","1-6","6-12","18-24"))
pm10$winddirection = factor(pm10$winddirection, levels=c(1,2,3,4),
                   labels=c("NE","SE","SW","NW"))

model3 = glm(highpm10 ~ log(cars), data = pm10, family = "binomial")
model4 = glm(highpm10 ~ log(cars) + time, data = pm10, family = "binomial")
summary(model3)
summary(model4)


AIC(model3)
AIC(model4)
AIC(model3, k=log(nrow(pm10)))
AIC(model4, k=log(nrow(pm10)))

###########
### 3.a ###
###########

# Now use all the variables to build an even better model. Which variables have a significant effect on
# the probability of PM10 exceeding 50 ??g/m3? Does the model seem reasonable, in particular the sign
# of the ??'s?
# traffic = 1; temperature = 2, windspeed = 3, wind-direction = 4

# since we dont have that many variables to experiment with, we can try all possible combinations:

m12 = glm(highpm10 ~ log(cars) + temp2m, data = pm10, family = "binomial")
m13 = glm(highpm10 ~ log(cars) + windspeed, data = pm10, family = "binomial")
m14 = glm(highpm10 ~ log(cars) + winddirection, data = pm10, family = "binomial")

summary(m14)
nn = log(nrow(pm10))
AIC1 = c(AIC(m12), AIC(m13), AIC(m14))
BIC1 = c(AIC(m12,k=nn), AIC(m13, k=nn), AIC(m14, k=nn))

#plot(c(1,2,3), AIC1, ylim=c(500, 540), xaxt = "n", type='l', col="red", xlab="model", ylab="BIC/AIC")
#axis(1, at=c(1,2,3), labels=c("X1+X2","X1+X3","x1+x4"))
#lines(c(1,2,3),BIC1, col="green")
# m13 is best of all two variable models

m123 = glm(highpm10 ~ log(cars) + temp2m + windspeed, data = pm10, family = "binomial")
m124 = glm(highpm10 ~ log(cars) + temp2m + winddirection, data = pm10, family = "binomial")
m134 = glm(highpm10 ~ log(cars) + windspeed + winddirection, data = pm10, family = "binomial")

AIC2 = c(AIC(m123), AIC(m124), AIC(m134))
BIC2 = c(AIC(m123,k=nn), AIC(m124, k=nn), AIC(m134, k=nn))

#plot(c(1,2,3), AIC2, ylim=c(500, 540), xaxt = "n", type='l', col="red", xlab="model", ylab="BIC/AIC")
#axis(1, at=c(1,2,3), labels=c("X1+X2+x3","X1+X2+X4","x1+X3+x4"))
#lines(c(1,2,3),BIC2, col="green")
# m123 is best of all 3-variable models.

m1234 = glm(highpm10 ~ log(cars) + temp2m + windspeed+winddirection, data = pm10, family = "binomial")
AIC3 = AIC(m1234)
BIC3 = AIC(m1234, k=nn)

# plotting everything together:
AIC = c(AIC1,AIC2,AIC3)
BIC = c(BIC1,BIC2,BIC3)

LAB = c("m12", "m13", "m14", "m123", "m124", "m134", "full")
plot(1:length(AIC), AIC, ylim=c(500, 550), xaxt = "n", type='l', col="red", xlab="model", ylab="BIC/AIC")
axis(1, at=1:length(AIC), labels=LAB)
lines(1:length(AIC),BIC, col="green")

AIC
BIC

m2 = glm(highpm10 ~ temp2m, data = pm10, family = "binomial")
summary(m2)
plot((pm10$cars) ~ (pm10$temp2m))
mm = lm(cars ~ log(temp2m), data = pm10)
summary(mm)
