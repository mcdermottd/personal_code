#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::: EDPSY 964 HLM: Level 1 and Level 2 Residuals ::::::::::
#::::::: & Empirical Bayes Estimators, Posterior Means :::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(lme4)    # lmer() for multilevel models, lmList() for regression for each school 
# library(languageR) # to get the p-values of the fixed effects
library(Hmisc)   # errbar
library(car)     # qqPlot
library(foreign) # read.spss & other package data files



# set working directory and read data file 
setwd("~/Dropbox/R")
NELS <-read.csv("NELS.with.dev.csv")


head(NELS)

(NELS$schid <- as.factor(as.numeric(NELS$schoolid)))

tail(NELS)
str(NELS)
dim(NELS)
summary(NELS)

# ::::: Random Intercept Models: Models 1 to 5 

# Model 1: empty model, a.k.a. null model or unconditional model
  (out1 <- lmer(math ~ 1 + (1|schid), NELS))

# Model 2 : level-1 predictor
  (out2 <- lmer(math ~ 1 + homework + (1|schid), NELS))

# Model 3 : level-2 predictor
  (out3 <- lmer(math ~ 1 + HW.groupmean + (1|schid), NELS))
  
# Model 4 : level-1 & level-2 predictors
  (out4 <- lmer(math ~ 1 + homework + HW.groupmean + (1|schid), NELS))

# Model 5 : group centered level-1 predictor & level-2 predictor 
  (out5 <- lmer(math ~ 1 + HW.within.dev + HW.groupmean + (1|schid), NELS))


# http://www.ssicentral.com/hlm/residual.html
# read Model 2 residuals 

l1res <- data.frame(read.spss("~/Dropbox/964/analysis/NELS.residual1.sav"))
l2res <- data.frame(read.spss("~/Dropbox/964/analysis/NELS.residual2.sav"))

l1res <- read.csv("~/Dropbox/964/analysis/NELSresidual1.csv")
l2res <- read.csv("~/Dropbox/964/analysis/NELSresidual2.csv")

dim(l1res)
str(l1res)
head(l1res)
tail(l1res)

dim(l2res)
str(l2res)
names(l2res)
l2res

# Level 1 Residuals
# 
for (i in levels(l1res$l2id)){plot(l1res$l1resid, l1res$fitval, data = l1res, color = i, add  = TRUE)}
plot(l1res$fitval, l1res$l1resid)
abline(h = 0)

plot(l1resid ~ fitval, data = l1res, col = l2id)
legend('topright', legend = unique(l1res$l2id), 
       bty = 'n', col = unique(l1res$l2id), pch = 1)
abline(h = 0)
# 8 colors       


colors =  c("black","red","green3","blue","cyan","magenta","yellow","gray","orange","violet")  

for (i in levels(l1res$l2id)){plot(l1res$l1resid, l1res$fitval, data = l1res, color = colors, add  = TRUE)}
plot(l1res$fitval, l1res$l1resid)
abline(h = 0)

plot(l1resid ~ fitval, data = l1res, col = l2id)
legend('topright', legend = unique(l1res$l2id), 
       bty = 'n', col = colors, pch = 1)
abline(h = 0)


qqPlot(l1res$l1resid)   #  package car


# Level 2 Residuals
# 
# If the normality assumption is true, then the Mahalanobis distances should be 
# distributed approximately chi-square. Analogous to univariate normal probability
# plotting, we can construct a Q-Q plot of mdist vs. chipct. 
# chipct are the expected values of the order statistics for a sample of size  
# selected from a population that is distributed . If the Q-Q plot resembles
# a 45 degree,  the random effects are distributed (multivariate) normal. 

plot(l2res$CHIPCT,l2res$MDIST)
abline(lm(MDIST ~ CHIPCT, l2res))
 

# Three estimates of the level-1 variability
# lntotvar: the natural logarithm of the total standard deviation within each unit.
# olsrsvar: the natural logarithm of the residual standard deviation within each unit
# based on its least squares regression. Note, this estimate exists only for those units
# which have sufficient data to compute level-1 OLS estimates.
# mdrsvar: the natural logarithm of the residual standard deviation from the final
# fitted fixed effects model. 

hist(l2res$LNTOTVAR)
hist(l2res$OLSRSVAR)
hist(l2res$MDRSVAR)

 
 
# EBINTRCPT & OLINTRCPT Empirical Bayes (U_0j^EB.hat) & OLS residual for the intercept
# "empirical Bayes shrinkage estimation"
plot(l2res$OLINTRCPT,l2res$EBINTRCPT)
plot(l2res$OLINTRCPT,l2res$EBINTRCPT, ylim = c(-10,10), xlim = c(-10,10))

# FVINTRCPT (beta0j.hat) 
l2res$FVINTRCPT

# FVHOMEWORK = gamma.10.hat = ECHOMEWORK for random intercept models 
# ECHOMEWORK vary acorss schools in random slope models 

# ECINTRCPT (beta0j.hat^EB) = beta0j.hat + U0j^EB: "Posterior means" 
round(cbind(l2res$ECINTRCPT, l2res$FVINTRCPT + l2res$EBINTRCPT),3)


# The posterior variances and covariances of the level-2 residuals 
# PV00: posterior variance of the intercept residual
# PV10: posterior covariance between the intercept residual and the slope residual
# PV11: posterior variance of the slope residual. 

l2res$PV0_0

# The posterior variances and covariances of the random Coefficient 
# PVC00 for the posterior variance of the random intercept
# PVC10 for the posterior covariance between the random intercept and the random slope
# PVC11 for the posterior variance of the random slope

l2res$PVC0_0



# ECINTRCPT (beta0j.hat^EB) = beta0j.hat + U0j^EB: "Posterior means" 
(postmean <- l2res$EBINTRCPT1)
(sd.postmean <- sqrt(l2res$PVC0_0))

(lower.b <- postmean - sd.postmean)
(upper.b <- postmean + sd.postmean)

# Order
(orderedL2 <- order(postmean, lower.b, upper.b))
(pm_sort <- postmean[orderedL2])
upper_sort <- upper.b[orderedL2]
lower_sort <- lower.b[orderedL2]

# A caterpillar plot of postmean
errbar(l2res$L2ID, pm_sort, upper_sort, lower_sort, xlab = 'Level 2 Units', ylab = 'Posterior Mean')  # package Hmisc

# A caterpillar plot of postmean + identify level-2 units
errbar(l2res$L2ID, pm_sort, upper_sort, lower_sort, col = l2res$L2ID, xlab = 'Level 2 Units', ylab = 'Posterior Mean', xlim = c(1,10))  # package Hmisc
legend('bottomright', legend = orderedL2, bty = 'n', col = l2res$L2ID, pch = 16)


colors =  c("black","red","green3","blue","cyan","magenta","yellow","gray","orange","violet")  
errbar(l2res$L2ID, pm_sort, upper_sort, lower_sort, col = colors, xlab = 'Level 2 Units', ylab = 'Posterior Means', xlim = c(1,10), xaxt ='n')  # package Hmisc
axis(side = 1, at = c(1:10), labels = orderedL2)
legend('bottomright', legend = orderedL2, bty = 'n', col = colors, pch = 16)

