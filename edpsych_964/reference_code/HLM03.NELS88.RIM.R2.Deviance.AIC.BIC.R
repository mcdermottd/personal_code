#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::   EDPSY 964 HLM: Random Intercept Models   ::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(lme4)   # lmer() for multilevel models, lmList() for regression for each school 

setwd("~/Dropbox/R")
NELS <-read.csv("NELS88.csv")

# ::::::::::: functions: RIvarcomp, means, reliability, R2 :::::::::


# extract variance components and compute ICC in two-level random intercept models
RIvarcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}

# arithmetic & harmonic means
means<-function(nj){
	a.mean <- mean(nj)
	h.mean <- length(nj)/sum(1/nj)
	c(arithmetic.mean = a.mean, harmonic.mean = h.mean)
}


# reliability of aggregated variables
reliability <- function(ICC, h.mean){
	lambda <- h.mean*ICC/(1+(h.mean-1)*ICC)
	reliability = lambda
}



# R2 type measures for explained variance: R2.1 & R2.2 
# R2.1: proportion reduction of error for predicting a level-1 outcome 
# R2.2: proportion reduction of error for predicting a group mean
# Input: tau's & sigma's from conditional & unconditional models
# + harmonic mean of level-1 units

R2 <- function(tauC, sigmaC, tauUC, sigmaUC, h.mean){ 
	R2.1 <- 1 - (tauC+sigmaC)/(tauUC+sigmaUC)
	R2.2 <- 1 - (tauC+sigmaC/h.mean)/(tauUC+sigmaUC/h.mean)
	c(R2.1 = R2.1, R2.2 = R2.2)
}


# Akaike's Information Criterion
# "AIC" & "BIC" are already used in R, so give them different names. 

AIC0 <- function(DEV, npar)
{
DEV+2*npar # model fit (-2LL) + penalty for model complexity
}

# Bayesian information criterion 
BIC0 <- function(DEV,npar,N)
{
DEV+npar*log(N)	
}

 

# ::::::::::::::::: END of functions ::::::::::::::::::::::::::::: 



# ::: Creating group means and within- and between-group deviation variables

NELS$schid <- as.factor(as.numeric(NELS$SCHOOLID))
NELS$math.groupmean <- tapply(NELS$math, NELS$schid, mean)[NELS$schid] 
NELS$HW.groupmean <- tapply(NELS$homework, NELS$schid, mean)[NELS$schid] 

NELS$HW.within.dev  <- NELS$homework-NELS$HW.groupmean
(HW.bet.dev         <- tapply(NELS$homework, NELS$schid, mean) - mean(NELS$homework))  
NELS$HW.between.dev <- HW.bet.dev[NELS$schid]

head(NELS)

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

vc.out1 <- RIvarcomp(out1)
vc.out2 <- RIvarcomp(out2)
vc.out3 <- RIvarcomp(out3)
vc.out4 <- RIvarcomp(out4)
vc.out5 <- RIvarcomp(out5)
(mm <- means(table(NELS$schid)))

rbind(vc.out1$var.component, vc.out2$var.component, vc.out3$var.component, vc.out4$var.component, vc.out5$var.component)

# Models 1 vs 3: increased residual variance! 


# Instead of traditional R2 that could decrease in multilevel models with adding variables, Snijders and Bosker suggest R2.1 & R2.2.  See the R2 function above. 

# Models 1 vs 2
R2(vc.out2$var.component[1],vc.out2$var.component[2],vc.out1$var.component[1],vc.out1$var.component[2],mm[2])

# Models 1 vs 3
R2(vc.out3$var.component[1],vc.out3$var.component[2],vc.out1$var.component[1],vc.out1$var.component[2],mm[2])

# Models 1 vs 5
R2(vc.out5$var.component[1],vc.out5$var.component[2],vc.out1$var.component[1],vc.out1$var.component[2],mm[2])



# :::: MODEL COMPARISONS: DEVIANCE, AIC, BIC :::::::::::: 
# FOR NESTED MODEL COMPARISONS, USE FIML NOT REML 

# Model 1: empty model, a.k.a. null model or unconditional model
  (out1.FIML <- lmer(math ~ 1 + (1|schid), NELS, REML = FALSE))

# Model. 2 : level-1 predictor
  (out2.FIML <- lmer(math ~ 1 + homework + (1|schid), NELS, REML = FALSE))

# Model 3 : level-2 predictor
  (out3.FIML <- lmer(math ~ 1 + HW.groupmean + (1|schid), NELS, REML = FALSE))
  
# Model 4 : level-1 & level-2 predictors
  (out4.FIML <- lmer(math ~ 1 + homework + HW.groupmean + (1|schid), NELS, REML = FALSE))

# Model 5 : group centered level-1 predictor & level-2 predictor 
  (out5.FIML <- lmer(math ~ 1 + HW.within.dev + HW.groupmean + (1|schid), NELS, REML = FALSE))


# Deviances between nested models (full vs. reduced models) 

getME(out5, "devcomp")               # REML
getME(out5, "devcomp")$cmp["dev"]    # dev: NA 


getME(out5.FIML, "devcomp")              # REML: NA 
getME(out5.FIML, "devcomp")$cmp["dev"]   # deviance  
getME(out5.FIML, "devcomp")$dims["p"]    # the number of parameters


dev1 <- getME(out1.FIML, "devcomp")$cmp["dev"]
dev2 <- getME(out2.FIML, "devcomp")$cmp["dev"]
dev3 <- getME(out3.FIML, "devcomp")$cmp["dev"]
dev4 <- getME(out4.FIML, "devcomp")$cmp["dev"]
dev5 <- getME(out5.FIML, "devcomp")$cmp["dev"]

# Likelihood-ratio test (LRT) for nested models
# TS = -2 ln (L.reduced / L.full) = -2LL.reduced - -2LL.full 
# Chi-squared tests, df = # parameters.full - # parameters.reduced

dev1-dev2
dev1-dev3
dev1-dev5
dev2-dev5
dev3-dev5

# models 2 and 3 are not tested 
# models 4 and 5 are statistically equivalent

anova(out1.FIML,out2.FIML) # model 2 > model 1 

getME(out2.FIML, "devcomp")
getME(out1.FIML, "devcomp")

# chisq = dev1-dev2 = 32.1215
# Df = difference in # parameters  = 2 - 1 = 1
# Pr (>Chisq)  = p-value 
1-pchisq(dev1-dev2, 2-1)
1-pchisq(dev1-dev5, 3-1)

anova(out3.FIML,out1.FIML) # model 3 > model 1, order in anova() doesn't matter 
anova(out1.FIML,out5.FIML) # model 5 > model 1

anova(out2.FIML,out5.FIML) # model 2 > model 5
anova(out3.FIML,out5.FIML) # model 5 > model 3
anova(out2.FIML,out3.FIML)  # NOT NESTED!
anova(out4.FIML,out5.FIML)  # NOT NESTED!

# model 2 is the best fitting model based on LRTs

# Summary of AIC, BIC, & logLik
anova(out1.FIML,out2.FIML,out3.FIML,out4.FIML,out5.FIML)

# Data: NELS
# Models:
# out1.FIML: math ~ 1 + (1 | schid)
# out2.FIML: math ~ 1 + homework + (1 | schid)
# out3.FIML: math ~ 1 + HW.groupmean + (1 | schid)
# out4.FIML: math ~ 1 + homework + HW.groupmean + (1 | schid)
# out5.FIML: math ~ 1 + HW.within.dev + HW.groupmean + (1 | schid)
#           Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# out1.FIML  3 1880.8 1891.5 -937.39   1874.8                             
# out2.FIML  4 1850.7 1864.9 -921.33   1842.7 32.121      1  1.448e-08 ***   # best fitting model
# out3.FIML  4 1878.2 1892.5 -935.11   1870.2  0.000      0          1    
# out4.FIML  5 1850.8 1868.7 -920.43   1840.8 29.363      1  6.001e-08 ***
# out5.FIML  5 1850.8 1868.7 -920.43   1840.8  0.000      0          1    
---

# anova()
anova(out5.FIML)           # F tests for the predictors 
anova(out1.FIML,out5.FIML) # AIC, BIC, deviance, chisq test

extractAIC(out5.FIML)  # npar & AIC
extractBIC(out5.FIML)  # no such function! 

# self made functions
AIC0(dev5,5)
BIC0(dev5,5,260)  #  sample size = the number of level 1 units
BIC0(dev5,5,10)   #  sample size = the number of level 2 units --- SAS mixed, nlmixed, glimmix
