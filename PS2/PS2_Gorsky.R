# PS2
# due 26 September, 2021 by midnight

# First, install the packages below if you haven't already got them on your computer. You won't need them all for chapter 1, but they are all used among the different chapters.

# install.packages(c("lme4", "lmerTest", "ape", "phylolm", "MCMCglmm", "mvtnorm", "car", "phytools", "phangorn", "logistf", "devtools", "phyr"))

# There is an additional package that are on github. To get them, you first have to load devtools
# devtools::install_github("arives/rr2")

# Once you have installed these packages, you won't need to do it again.

# Packages you will need for Chapter 1:
library(lme4)
library(lmerTest)
library(car)

# This problem set begins to explore the grouse data. It is based on correlated_data Ch1 sections 1.1 to 1.4.

# For the homework, ONLY TURN IN THE R CODE THAT YOU USED. Start with the code from correlated_data ("Ch_1_10Aug18.R") which is below and add to it anything you need. Identify you new code (so I can find it) by placing it between marker rows #~~~~~~~~~~~~~~~~~~~~~~~~~~~. I will get your full answers to the questions by randomly asking people in class, so there is no need for you to write them down.

# 1. In figure 1.1 (left panel), it looks like there is spatial correlation in the numbers of grouse scored per route (even though there isn't). Is there a simple way you could test for spatial correlation? (NOTE: This might require NO NEW R CODE.)

# 2. What are the assumptions made for a linear model (LM) in section 1.4.1? Which of these assumptions are likely to be violated when applying a LM in regressing MEAN_GROUSE on MEAN_WIND? Which of these violations do you think leads to greatest concerns about the P-values produced in the regression? (NOTE: This might require NO NEW R CODE.)

# 3. Perform a Binomial GLM using a probit link function rather than a logit link function in the analyses in section 1.4.2. Are the results the same? Can you explain the differences? Which one is correct?

# 4. In section 1.4.5 there is a simulation for the GLMM model. How could you use this to test whether the P-values given by the LM (section 1.4.1) are correct? If you want, for bonus points give it a try.

# 5. What questions do you have about the material in section 1.1 to 1.4? What needs more explanation? I'm serious about asking this question, because I want to improve the book. (NOTE: This requires NO NEW R CODE.)

# BONUS

# 6. Another method that could be used with these data is a beta binomial model. For bonus points, give this a try. You will need library(glmmTMB).


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# code from Ch_1_10Aug18.R

#################################################################
# 1.3 Dataset
#################################################################

# Metadata for "grouse_data.csv"

# ROUTE
# IDs for 50 roadside routes. These data are simulated to have similar characteristics as the original, real data.

# STATION
# IDs for each survey station, with up to 8 STATIONS per ROUTE.

# LAT
# X coordinate of survey station. UTM zone 15N NAD83 datum.

# LONG
# Y coordinate of survey station. UTM zone 15N NAD83 datum.

# WIND
# Wind speed (km/hour) recorded at 1.4m above ground at the end of the survey.

# TEMP
# Temperature (°C) recorded at 1.4m above ground at the end of the survey.

# GROUSE
# Detection/non-detection of Ruffed Grouse (1 = detected, 0 = not detected).

# read data
d<- read.csv('PS1/grouse_data.csv', header = T)

# STATION and ROUTE were uploaded as integers; this converts them to factors.
d$STATION <- as.factor(d$STATION)
d$ROUTE <- as.factor(d$ROUTE)

# To see the first 20 rows of data, you can use:
head(d, 20)

# Combine all values for each ROUTE.
w <- data.frame(aggregate(cbind(d$GROUSE, d$WIND, d$LAT, d$LONG), by = list(d$ROUTE), FUN = mean))

# For clarity, I've added the column names.
names(w) <- c("ROUTE","MEAN_GROUSE","MEAN_WIND","LAT","LONG")

# Finally, I want the count of the number of STATIONS per ROUTE, and the number of GROUSE.
w$GROUSE <- aggregate(d$GROUSE, by = list(d$ROUTE), FUN = sum)[,2]
w$STATIONS <- aggregate(array(1, c(nrow(d), 1)), by = list(d$ROUTE), FUN = sum)[,2]

# This is what the aggregated data look like:
head(w)

# Fig. 1.1
# To take a look at the data, this plots MEAN_GROUSE against LAT and LONG, using the size of the point to give the mean number of observations per route. "cex" gives the point size, and I've added 0.1 to w$MEAN_GROUSE so that routes with zero counts are still shown
par(mfrow=c(1,3))
plot(LAT ~ LONG, data=w, pch=1, cex=5*(MEAN_GROUSE+.1), xaxt="n", yaxt="n", xlab="Longitude", ylab="Latitude")

# This histogram shows the variability in grouse counts per ROUTE.
hist(w$MEAN_GROUSE, main="", xlab="MEAN_GROUSE per ROUTE")

# It looks like there are fewer GROUSE seen when the wind speed is higher
plot(MEAN_GROUSE ~ MEAN_WIND, data=w)

#################################################################
# 1.4 Route-level analyses 
#################################################################

# Packages you will need:
library(lme4)
library(lmerTest)

######
# 1.4.1 LM at the route level: Analyses at the ROUTE level using an arcsine-square-root transform
summary(lm(asin(sqrt(MEAN_GROUSE)) ~ MEAN_WIND, data=w))

######
# 1.4.2 Binomial GLM at the route level

# For the binomial GLM, it is necessary to construct a variable that contains the successes (number of stations with GROUSE) and failures (number of stations without GROUSE). This accounts for the different numbers of STATIONS among ROUTES. I have called this array "SUCCESS".
w$SUCCESS <- cbind(w$GROUSE, w$STATIONS - w$GROUSE)
summary(glm(SUCCESS ~ MEAN_WIND, family = binomial, data=w))

#~~~~~~~~~~~~~~~~
summary(glm(SUCCESS ~ MEAN_WIND, family = binomial(link = "probit"), data=w))
mod.probitf <- glm(SUCCESS ~ MEAN_WIND, family = binomial(link = "probit"), data=w)
mod.probitr <- glm(SUCCESS ~ 1, family = binomial(link = "probit"), data=w)
deviance.probit <- 2*(logLik(mod.probitf) - logLik(mod.probitr))
LRT.bprobit1 <- c(dev = deviance.probit, p.value=pchisq(deviance.probit, df=1, lower.tail=F))
LRT.bprobit1
#statistically no reason to use one verse another

#Binomial model (GLM) with size > 1
SUCCESS<- cbind(success, failure)
z<- glm(SUCCESS ~ X, family = binomial)
#Normal- binomial model
obs<- 1:n
z<- glmer(SUCCESS ~ X + (1|obs), family = binomial)

# different p-values and estimate given 8 model choices <- CH2 gives you the tools to assess quality
#~~~~~~~~~~~~~~~~

# Likelihood Ratio Test for b1.
mod.f <- glm(SUCCESS ~ MEAN_WIND, family = binomial, data=w)
mod.r <- glm(SUCCESS ~ 1, family = binomial, data=w)
deviance <- 2*(logLik(mod.f) - logLik(mod.r))
LRT.b1 <- c(dev = deviance, p.value=pchisq(deviance, df=1, lower.tail=F))
LRT.b1

######
# 1.4.3 Quasi-binomial GLM at the route level: This model looks exactly like the binomial GLM, although the family is changed to quasibinomial.
summary(glm(SUCCESS ~ MEAN_WIND, family = quasibinomial, data=w))

######
# 1.4.4 Logit-normal binomial GLMM at the route level: This GLMM has "observation-level variation", with each ROUTE allowed to take a different value from a normal distribution. This technically produces a logit normal-binomial distribution.
summary(glmer(SUCCESS ~ MEAN_WIND + (1 | ROUTE), data=w, family=binomial))

######
# 1.4.5 Simulation of overdispersion
# take this simulaiton code and trying it out for the LM

# This is the inverse logit function
inv.logit <- function(x){
  1/(1 + exp(-x))
}

# Simulate the logit normal-binomial GLMM. Note that samples = 1000 points are simulated.
b0 <- 0
b1 <- 0
n <- 8

samples <- 1000
x <- rnorm(samples, mean=0, sd=1)

sd.e <- 0
e <- rnorm(samples, mean=0, sd=sd.e)

Z <- b0 + b1 * x + e
p <- inv.logit(Z)
Y <- rbinom(samples, size=n, prob=p)

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

#########################################################
# Reiteration of results
#########################################################

# Route-level methods
summary(lm(asin(sqrt(MEAN_GROUSE)) ~ MEAN_WIND, data=w))$coef

w$SUCCESS <- cbind(w$GROUSE, w$STATIONS - w$GROUSE)
summary(glm(SUCCESS ~ MEAN_WIND, family = binomial, data=w))$coef

summary(glm(SUCCESS ~ MEAN_WIND, family = quasibinomial, data=w))$coef

summary(glmer(SUCCESS ~ MEAN_WIND + (1 | ROUTE), data=w, family=binomial))$coef



