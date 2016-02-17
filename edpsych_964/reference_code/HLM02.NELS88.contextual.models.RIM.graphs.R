#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::: EDPSY 964 HLM: Contextual Models with NELS88 ::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# load R packages

library(lme4)      # Linear mixed-effects models using S4 classes. lmer() for multilevel models, 
# library(languageR) # to get the p-values of the fixed effects
# package lmerTest also performs different kinds of tests on lmer objects
#blibrary(Hmisc)     # error bar, errbar()

# set working directory and read data file 


# ::::::::::: functions: RIvarcomp, means, reliability :::::::::

# extract variance components and compute ICC in two-level random intercept models
# help(lmer), click the "mer" class to see "Slots" 

RIvarcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}


# arithmetic & harmonic.means
means<-function(nj){
	a.mean <- mean(nj)
	h.mean <- length(nj)/sum(1/nj)
	c(arithmetic.mean = a.mean, harmonic.mean = h.mean)
	}


# reliability of aggregated variables
reliability <- function(ICC, h.mean){
	lambda <- h.mean*ICC/(1+(h.mean-1)*ICC)
	c(reliability = lambda)
}

# ::::::::::::::::: END of functions ::::::::::::::::::::::::::::: 

setwd("~/Dropbox/R")
NELS <-read.csv("NELS88.csv")

head(NELS)
tail(NELS)

str(NELS)
dim(NELS)
summary(NELS)

(NELS$schid <- as.factor(as.numeric(NELS$schoolid)))

NELS[1:25,]

# attach data
attach(NELS)

# ::::::::::::::::::  plots & regression lines :::::::::::::::::::

plot(schid, jitter(math), xlab = 'School ID', ylab = 'Math score')   
plot(as.numeric(schid), jitter(math), xlab = 'School ID', ylab = 'Math score')   

plot(schid, jitter(homework), xlab = 'School ID', ylab = 'Math score')   
plot(as.numeric(schid), jitter(homework), xlab = 'School ID', ylab = 'Math score')  

plot(jitter(homework), math, xlab = 'Homework', ylab = 'Math score')                
for(i in levels(schid)){                     #  regression line for each school
    out.lm <- lm(math ~ homework, data = NELS, sub = schid == i)
    abline(out.lm)
}

layout(matrix(c(1,5,0,2,6,9,3,7,10,4,8,0),3,4))
layout.show(10)
for(i in 1:10) { 
	inds = which(NELS$schid == i)
	plot(NELS$homework[inds], NELS$math[inds], xlab = "homework", ylab = "Math", ylim = c(30,75),
	main = paste ("School", unique(NELS$schoolid)[i], sep=' '))
	simMod = lm(math ~ homework, data = NELS, sub = (schid == i))
	abline(simMod, lty=2)
}

# par(mfrow=c(1,1))
detach(NELS)

# ::::::::: create level-2 variables from level-1 variables :::::::::

(N.st    <- table(NELS$schid))                    # the number of students in each school
(HW.gm   <- tapply(NELS$homework, NELS$schid, mean)) # homework group mean
(math.gm <- tapply(NELS$math, NELS$schid, mean))     # math group mean

cbind(NELS$schid, N.st,HW.gm,math.gm)          # N = 260 (# level 1 units) rows
cbind(unique(NELS$schid), N.st,HW.gm,math.gm)  # M = 10 (# level 2 units) rows


#::::: attach new level-2 variables to data 
N.st
N.st[NELS$schid]

NELS$N.students     <- N.st[NELS$schid]   
NELS$HW.groupmean   <- HW.gm[NELS$schid]
NELS$math.groupmean <- math.gm[NELS$schid]

head(NELS)

# tapply & attach together in one sentence
NELS$math.schmean <- tapply(NELS$math, NELS$schid, mean)[NELS$schid]   # same as math.groupmean

head(NELS, 30)

# create within and between group deviatons

NELS$HW.within.dev  <- NELS$homework-NELS$HW.groupmean
(HW.bet.dev         <- HW.gm - mean(NELS$homework))  
NELS$HW.between.dev <- HW.bet.dev[NELS$schid]

tail(NELS,30)


# write.csv(NELS,"NELS.w.dev.csv")


# :::::::::: Fixed effects ANOVA :::::::::::::::

summary(aov(math ~ schid, NELS))  # Ho: all population means are equal. 
summary(lm(math ~ schid, NELS))   # default: the first school is the reference group 

NELS$schid <- relevel(NELS$schid, ref = '10') # use school10 as the reference group
summary(lm(math ~ schid, NELS))

NELS$schid <- relevel(NELS$schid, ref = '7') # use school 7 as the reference group
summary(lm(math ~ schid, NELS))


# :::: Random effects ANOVA: Ho: variance(schoolID) = tau^2 = tau00 = 0 ::::
# H1 = tau00 > 0 
 
summary(RE.ANOVA.out <- lmer(math ~ 1 + (1|schoolid), NELS))    # empty model
# a.k.a. null model or unconditional model, one factor random effects ANOVA model  

(varcomp.math <- RIvarcomp(RE.ANOVA.out))


# :::::::: Contextual models :::::::::::::::

summary(lm(math ~ homework, NELS))                      # total or pooled regression
summary(lm(math.gm ~ HW.gm, weights = N.st, NELS))      # aggregate regression
summary(lm(math ~ homework + HW.groupmean, NELS))       # contextual model
summary(lm(math ~ HW.within.dev + HW.between.dev, NELS))# Cronbach model 
summary(lm(math ~ schoolid + homework, NELS))           # ANCOVA
summary(lmList(math ~ homework | schoolid, NELS))       # regression for each school


# relationship among b.total, b.within, & b.between
# b.total = eta2*b.between + (1 - eta2)*b.within

# Reliability of aggregated values (e.g., mean math score, mean HW score for schools) 
# lambda = var(true scores)/var(observed scores) 
# In the pobpulation, 
# beta.total = rho.I*beta.between + (1-rho.I)*beta.within

# rho.I = tau2/(tau2 + sigma2)
# lambda.j = nj*rho.I / (1 + (nj - 1)*rho.I)
# whan nj = 1, lambda.j = rho.I
# as nj increases, lambda.j -> 1, eta2 = rho.I/lambda.j -> rho.I

# homework as outcome 
summary(out.lm <- lm(homework~schoolid,NELS))
(HW.corr.ratio <- summary(out.lm)$r.squared)    
# .294

(b.between <- summary(lm(math.gm ~ HW.gm, weights = N.st))$coefficients[2,1])
(b.within  <- summary(lm(math ~ homework + HW.groupmean, NELS))$coefficients[2,1])
# b.between <- 7.015
# b.within  <- 2.137

HW.corr.ratio*b.between+(1-HW.corr.ratio)*b.within
# b.total = 3.572

summary(HW.out <- lmer(homework ~ 1 + (1|schoolid), NELS))    # empty model
(varcomp.HW <-RIvarcomp(HW.out))       


# arithmetic vs. harmonic.means

sort(N.st)

(mm <- means(N.st))


# reliability of aggregated variables

reliability(varcomp.math$ICC,mm["harmonic.mean"])    # .915
reliability(varcomp.HW$ICC,mm["harmonic.mean"])      # .857

reliability(varcomp.math$ICC,mm["arithmetic.mean"])  # overestimated lambda  .924
reliability(varcomp.HW$ICC,mm["arithmetic.mean"])    # overestimated lambda  .872 

reliability(varcomp.math$ICC, N.st)
mean(reliability(varcomp.math$ICC, N.st))            #.9155 close to h.mean lambda
reliability(varcomp.HW$ICC, N.st)
mean(reliability(varcomp.HW$ICC, N.st))              #.858 close to h.mean lambda


###########################################################
###### Multilevel Models/Hierarchical Linear Models  ######
###########################################################

# Model 1: empty model, a.k.a. null model or unconditional model
  (out1 <- lmer(math ~ 1 + (1|schid), NELS))
# pvals.fnc(out1)
# two p-values, one based on the posterior distribution (pMCMC) and one based on the t-distribution

# Model 2 : level-1 predictor
  (out2 <- lmer(math ~ 1 + homework + (1|schid), NELS))

# Model 3 : level-2 predictor
  (out3 <- lmer(math ~ 1 + HW.groupmean + (1|schid), NELS))
  
# Model 4 : level-1 & level-2 predictors
  (out4 <- lmer(math ~ 1 + homework + HW.groupmean + (1|schid), NELS))

# Model 5 : group centered level-1 predictor & level-2 predictor 
  (out5 <- lmer(math ~ 1 + HW.within.dev + HW.between.dev + (1|schid), NELS))

RIvarcomp(out1)  # ICC
RIvarcomp(out5)  # residual ICC


# :::::::: Plausible value range for school means: gamma.hat -/+ t.cv*sqrt(tau) ::::::::::

# Model 1 
fixef(out1)
VarCorr(out1)$schid[1,1]
qt(.975,9)  # t.cv 

c(lower.bound <- fixef(out1)-qt(.975,9)*sqrt(VarCorr(out1)$schid[1,1]), upper.bound <- fixef(out1)+qt(.975,9)*sqrt(VarCorr(out1)$schid[1,1]))
upper.bound-lower.bound

# Model 3: HW.groupmean = 0 

fixef(out3)
fixef(out3)[1]
VarCorr(out3)$schid[1,1]
qt(.975,8)

c(lower.bound <- fixef(out3)[1]-qt(.975,8)*sqrt(VarCorr(out3)$schid[1,1]), upper.bound <- fixef(out3)[1]+qt(.975,8)*sqrt(VarCorr(out3)$schid[1,1]))
upper.bound-lower.bound

# Model 3: HW.groupmean = 2.023 
mean(NELS$HW.groupmean) 

fixef(out3)
fixef(out3)%*%c(1, 2.023)
qt(.975,8)
VarCorr(out3)$schid[1]

c(lower.bound <- fixef(out3)%*%c(1, 2.023)-qt(.975,8)*sqrt(VarCorr(out3)$schid[1]),  upper.bound <- fixef(out3)%*%c(1, 2.023)+qt(.975,8)*sqrt(VarCorr(out3)$schid[1]))


# same as

c(lower.bound <- fixef(out3)[1]+fixef(out3)[2]*2.023-qt(.975,8)*sqrt(VarCorr(out3)$schid[1]), upper.bound <- fixef(out3)[1]+fixef(out3)[2]*2.023+qt(.975,8)*sqrt(VarCorr(out3)$schid[1]))

upper.bound-lower.bound



# ::: S & B (2012) pp.64-67 Posterior confidence intervals 
re.out5 <- ranef(out5, postVar=TRUE, standard=TRUE)
str(re.out5)
(postmean <- re.out5$schid[,1])

# and the posterior variances are
(postvar <-  attr(re.out5$schid,'postVar')[1,1,])

# These are also the comparative variances.
# The diagnostic variance is calculated using (4.18):
# for model checking (i.e., EB / dig sd  = the standardized empirical Bayes estimators)
(diagvar <- VarCorr(out5)$schid[1,1] - postvar)

# Comparative standard error (a.k.a. posterior standard deviations of U_0j)
# to assess how well the U_0j can be estimated from data to compare levle-2 units with each other
compsd <- sqrt(postvar)

# random coefficient variance tau_00 or tau_0^2 = comparative variance + diag var

# Bounds of comparative intervals
# compare the equality of level-2 units at the 5 % significance  level 
# 1.39 instead of 1.96 (thus comparative CIs are smaller than CI for assessing single groups)
# See Goldstein & Healy (1995). JRSS, A, 'The graphical representation of a collection of means'.   
lower <- postmean - 1.39*compsd
upper <- postmean + 1.39*compsd

# Order
perm <- order(postmean, lower, upper)
pm_sort <- postmean[perm]
upper_sort <- upper[perm]
lower_sort <- lower[perm]


# A caterpillar plot can be produced as follows.
par(mfrow=c(1,1))
errbar(1:10, pm_sort, upper_sort, lower_sort)