# Packages you will need for PS2:
library(lme4)
library(lmerTest)
library(car)

# read data
d<- read.csv('PS1/grouse_data.csv', header = T)

# STATION and ROUTE were uploaded as integers; this converts them to factors.
d$STATION <- as.factor(d$STATION)
d$ROUTE <- as.factor(d$ROUTE)

# Combine all values for each ROUTE.
w <- data.frame(aggregate(cbind(d$GROUSE, d$WIND, d$LAT, d$LONG), by = list(d$ROUTE), FUN = mean))

# For clarity, I've added the column names.
names(w) <- c("ROUTE","MEAN_GROUSE","MEAN_WIND","LAT","LONG")

# Finally, I want the count of the number of STATIONS per ROUTE, and the number of GROUSE.
w$GROUSE <- aggregate(d$GROUSE, by = list(d$ROUTE), FUN = sum)[,2]
w$STATIONS <- aggregate(array(1, c(nrow(d), 1)), by = list(d$ROUTE), FUN = sum)[,2]

#PS2
#Question 3
# 3. Perform a Binomial GLM using a probit link function rather than a logit link function in the analyses in section 1.4.2. Are the results the same? Can you explain the differences? Which one is correct?

# Binomial GLM with a logit link function
w$SUCCESS <- cbind(w$GROUSE, w$STATIONS - w$GROUSE)
summary(glm(SUCCESS ~ MEAN_WIND, family = binomial, data=w))

#~~~~~~~~~~~~~~~~
# Binomial GLM with a probit link function
summary(glm(SUCCESS ~ MEAN_WIND, family = binomial(link = "probit"), data=w))
# Fisher Scoring iterations is one less
#~~~~~~~~~~~~~~~~

# Likelihood Ratio Test for b1.
mod.f <- glm(SUCCESS ~ MEAN_WIND, family = binomial, data=w)
mod.r <- glm(SUCCESS ~ 1, family = binomial, data=w)
deviance <- 2*(logLik(mod.f) - logLik(mod.r))
LRT.b1 <- c(dev = deviance, p.value=pchisq(deviance, df=1, lower.tail=F))
LRT.b1
#~~~~~~~~~~~~~~~~
#Likelihood Ratio Test for probit link function
mod.probitf <- glm(SUCCESS ~ MEAN_WIND, family = binomial(link = "probit"), data=w)
mod.probitr <- glm(SUCCESS ~ 1, family = binomial(link = "probit"), data=w)
deviance.probit <- 2*(logLik(mod.probitf) - logLik(mod.probitr))
LRT.bprobit1 <- c(dev = deviance.probit, p.value=pchisq(deviance.probit, df=1, lower.tail=F))
LRT.bprobit1
#~~~~~~~~~~~~~~~~
#statistically no reason to use one verse another
#Slightly higher p-value and lower deviance for binomial GLM using a probit link function
#P-value from summary though when the errors are not normally distributed is an approximation

# 4. In section 1.4.5 there is a simulation for the GLMM model. How could you use this to test whether the P-values given by the LM (section 1.4.1) are correct? If you want, for bonus points give it a try.

# Simulate the LM. Note that samples = 1000 points are simulated.
b0 <- 0
b1 <- 0
n <- 8

samples <- 1000
x <- rnorm(samples, mean=0, sd=1)

sd.e <- 0
e <- rnorm(samples, mean=0, sd=sd.e)

Z <- b0 + b1 * x + e
Y <- lm(Z)

# If b1 = 0, this gives the expected standard deviation of the binomial distribution
p <- inv.logit(b0)
expected.var <- round(n*p*(1-p),2)
observed.var <- round(var(Y), 2)

par(mfcol=c(2,1), mai=c(.8,.8,.2,.01))

plot(Z[order(x)] ~ x[order(x)], main="", xlab="x", ylab="Z")
lines(b0 + b1 * x[order(x)] ~ x[order(x)], col="red")

plot(Y[order(x)] ~ x[order(x)], ylim=c(0,n), xlab="x", ylab="Y", main=paste("expected =", expected.var,"observed =", observed.var), cex.main=1)
x <- .1*(-40:40)
lines(n*inv.logit(b0 + b1*x) ~ x, col="red")

summary(glmer(SUCCESS ~ MEAN_WIND + (1 | ROUTE), data=w, family=binomial))
