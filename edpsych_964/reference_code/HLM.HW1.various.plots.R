#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::: EDPSY 964 HLM: Contextual Models with NELS88 ::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# load R packages

library(lme4)      # Linear mixed-effects models using S4 classes. lmer() for multilevel models, 
# library(languageR) # to get the p-values of the fixed effects
# package lmerTest also performs different kinds of tests on lmer objects
library(Hmisc)     # errorbar
library(ggplot2)   # ggplot
library(car)       # scatterplot 

# set working directory and read data file 

# setwd("~/Dropbox/964/analysis")
setwd("C:/Users/Drew/Dropbox/course_data/reference")
NELS <- read.csv("NELS88.csv")

head(NELS)
tail(NELS)

# (NELS$schid <- as.factor(as.numeric(NELS$schoolid)))
(NELS$schid <- as.factor(as.numeric(NELS$schnum)))



# ::::::::::: functions: varcomp, harmonic mean, reliability :::::::::

# extract variance components and compute ICC in two-level random intercept models
# help(lmer), click the "mer" class to see "Slots" 
varcomp <- function(obj) {
  components <- unlist(lapply(VarCorr(obj), diag))
  residual <- attr(VarCorr(obj), "sc")
  variance_components <- c(components, Residual = residual ^ 2)
  ICC <- components / sum(variance_components)
  list(var.components = variance_components, ICC = ICC)
}


# harmonic.mean

h.mean <- function(x){length(x)/sum(1/x)}


# reliability of aggregated variables
reliability <- function(ICC, harmonic.mean){
	harmonic.mean*ICC/(1+(harmonic.mean-1)*ICC)
}

# ::::::::::::::::: END of functions ::::::::::::::::::::::::::::: 

str(NELS)
dim(NELS)
summary(NELS)


NELS[1:50,]

# attach data
# attach(NELS)


# Q1
# plots & regression lines

plot(jitter(NELS$ses), NELS$math, xlab = 'SES', ylab = 'Math score', main = "Math vs SES")

# Total sample of math~ses, colored by school
colors =  c("violet","magenta","red","blue","cyan","green3","yellow","orange","black","gray")

plot(jitter(NELS$ses), NELS$math, xlab = 'SES', ylab = 'Math score', 
      main = c("Math vs SES", "(colored by school)"),
      col = colors[as.numeric(NELS$schid)],
      pch = 16)
 
# 10 separate scatterplots for each school, math vs ses
layout(matrix(c(1,5,0,2,6,9,3,7,10,4,8,0),3,4))
layout.show(10)
for(i in levels(NELS$schid)){                     
   plot(NELS$math ~ jitter(NELS$ses), 
        main = c('School', i),
        xlab = 'SES', xlim = c(-2,2), 
        ylab = 'Math score', ylim = c(30,70), subset = NELS$schid == i)
   out.lmc <- lm(math ~ ses, data = NELS, subset = schid == i)
   abline(out.lmc)
} 


(plot.all <-ggplot(NELS, aes(x = ses, y = math)) + geom_point() + theme_bw())
plot.all+stat_smooth(method="lm", alpha = 0.15)

(plot.by.school <- plot.all + facet_wrap(~schid, nrow=2))

plot.by.school+stat_smooth(method="lm", alpha = 0.15)


xyplot(math ~ ses | schid, data = NELS, type = c("p", "r"))


par(mfrow = c(1,1))
plot(NELS$schid, jitter(NELS$math), xlab = 'School ID', ylab = 'Math score')   
plot(as.numeric(NELS$schid), jitter(NELS$math), xlab = 'School ID', ylab = 'Math score')   

plot(NELS$schid, jitter(NELS$homework), xlab = 'School ID', ylab = 'Math score')   
plot(as.numeric(NELS$schid), jitter(NELS$homework), xlab = 'School ID', ylab = 'Math score')  

plot(jitter(NELS$homework), NELS$math, xlab = 'Homework', ylab = 'Math score')                
for(i in levels(NELS$schid)){                     #  regression line for each school
    out.lm <- lm(math ~ homework, data = NELS, sub = schid == i)
    abline(out.lm)
}


par(mfrow = c(1,2))
plot(jitter(NELS$ses), NELS$math, xlab = 'SES', ylab = 'Math score', main = "total regression", col = colors)                
out.totalreg <- lm(math ~ ses, NELS)
abline(out.totalreg)

plot(jitter(NELS$ses), NELS$math, xlab = 'SES', ylab = 'Math score', main = "regression for each school", col = colors)                
for(i in levels(NELS$schid)){                     #  regression line for each school
    out.lm <- lm(math ~ homework, data = NELS, sub = schid == i)
    abline(out.lm)
}



ggplot(NELS, aes(x = ses, y = math)) + geom_point() + facet_wrap("schid") + stat_smooth(method="lm")

ggplot(NELS, aes(x = ses, y = math, color = schid)) + geom_point() + stat_smooth(method="lm", se = FALSE)


scatterplot(NELS$math ~ NELS$ses)

# scatterplot(NELS$math ~ NELS$ses, groups = NELS$schid, legend.coords ="topleft")
# scatterplot(NELS$math ~ jitter(NELS$ses), subset(NELS, NELS$schid == 7))


# Q2
# ::::: create level-2 variables from level-1 variables

(N.st    <- table(NELS$schid))                  # the number of students in each school
(ses.gm  <- tapply(NELS$ses, NELS$schid, mean)) # homework group mean
(math.gm <- tapply(NELS$math, NELS$schid, mean))     # math group mean

cbind(NELS$schid, N.st, ses.gm,math.gm)          # N (# level 1 units) rows
cbind(unique(NELS$schid), N.st, ses.gm,math.gm)  # M (# level 2 units) rows


#::::: attach new level-2 variables to data 

NELS$N.students     <- N.st[NELS$schid]   
NELS$ses.groupmean  <- ses.gm[NELS$schid]
NELS$math.groupmean <- math.gm[NELS$schid]

head(NELS)

# tapply & attach together in one sentence
# NELS$math.schmean <- tapply(NELS$math, NELS$schid, mean)[NELS$schid]   # same as math.groupmean


# create within and between group deviatons


NELS$ses.within.dev  <- NELS$ses-NELS$ses.groupmean
(ses.bet.dev         <- ses.gm - mean(NELS$ses))  
NELS$ses.between.dev <- ses.bet.dev[NELS$schid]

head(NELS)


# write.csv(NELS,"NELS.ses.w.dev.csv")


mean(NELS$ses)
sd(NELS$ses)  # 0.9704347  
range(NELS$ses)

ses.gm
mean(ses.gm)
sd(ses.gm)    # 0.5946607
range(ses.gm)


summary(lm(formula = math ~ ses, data = NELS))


#            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  51.8225     0.5438   95.29   <2e-16 ***
# ses           7.1276     0.5599   12.73   <2e-16 ***

summary(lm(formula = math ~ ses.within.dev + ses.between.dev, data = NELS))

#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      51.3000     0.5100 100.590  < 2e-16 ***
# ses.within.dev    3.6265     0.7941   4.567 7.68e-06 ***
# ses.between.dev   9.8748     0.7034  14.038  < 2e-16 ***


# 3.6265*.9704 = 3.519156 (1 SD), 7.038311 (2 SD) 
# 9.8748*.59466 = 5.872149 (1 SD),  2.936074 (1/2 SD)


# Q3
# Correlation Ratio
summary(out.ses <- lm(ses~schid,NELS))
(ses.corr.ratio <- summary(out.ses)$r.squared) 
# 0.5603327


# Null or Empty model
(out.ses.null <- lmer(ses ~ 1 + (1|schid), NELS))
varcomp(out.ses.null)
# schid.(Intercept)          Residual 
#        0.3480525         0.4286798 

# $ICC
# schid.(Intercept) 
        0.4480984 



# Q4
(b.between <- summary(lm(math.gm ~ ses.gm, weights = N.st))$coefficients[2,1])
# 9.874753

(b.within  <- summary(lm(math ~ ses + ses.groupmean, NELS))$coefficients[2,1])
# 3.626537

ses.corr.ratio*b.between+(1-ses.corr.ratio)*b.within
# 7.127616


# Q5

summary(lmList(math ~ ses | schid, NELS))  

# Call:
#   Model: math ~ ses | NULL 
#    Data: NELS 
# 
# Coefficients:
#    (Intercept) 
#    Estimate Std. Error  t value Pr(>|t|)
# 1  48.34456   2.388327 20.24202        0
# 2  50.80327   1.837426 27.64916        0
# 3  46.48478   3.251279 14.29738        0
# 4  64.13072   2.469690 25.96711        0
# 5  50.50734   1.791233 28.19697        0
# 6  48.53921   2.021613 24.01014        0
# 7  47.52578   2.354843 20.18215        0
# 8  41.25208   3.064769 13.46009        0
# 9  52.44481   1.675770 31.29594        0
# 10 52.94300   2.716069 19.49251        0
#    ses 
#      Estimate Std. Error     t value    Pr(>|t|)
# 1   6.8469661   2.361746  2.89911191 0.004088952
# 2   4.7741286   3.262328  1.46341164 0.144663211
# 3   0.0793475   2.530469  0.03135683 0.975011027
# 4  -1.2538648   2.167380 -0.57851647 0.563457958
# 5   4.9175599   1.882065  2.61285305 0.009545920
# 6   4.7657780   2.140879  2.22608445 0.026937765
# 7   3.7020714   3.411243  1.08525594 0.278897214
# 8  -1.0588654   2.920285 -0.36258974 0.717230476
# 9   5.4743512   2.054736  2.66425997 0.008238817
# 10 10.0951431   4.021776  2.51012099 0.012728166


# End of HW #1 



# Fixed effects ANOVA

summary(aov(math ~ schid, NELS))  # Ho: all population means are equal. 

NELS$schoolid <- relevel(NELS$schid, ref = '10') # use school10 as the reference group
summary(lm(math ~ schoolid, NELS))

NELS$schoolid <- relevel(NELS$schid, ref = '7') # use school 7 as the reference group
summary(lm(math ~ schoolid, NELS))


# Random effects ANOVA: Ho: variance(schoolID) = tau^2 = tau00 = 0

summary(RE.ANOVA.out <- lmer(math ~ 1 + (1|schoolid), NELS))    # empty model
# a.k.a. null model or unconditional model, one factor random effects ANOVA model  

(varcomp.math <- varcomp(RE.ANOVA.out))


# Contextual models

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
# as nj increases, lambda.j -> 1, eta2 -> rho.I

# homework as outcome 
summary(out.lm <- lm(homework~schoolid,NELS))
(HW.corr.ratio <- summary(out.lm)$r.squared)    # help(lm) to see "Value" 
# .294

(b.between <- summary(lm(math.gm ~ HW.gm, weights = N.st))$coefficients[2,1])
(b.within  <- summary(lm(math ~ homework + HW.groupmean, NELS))$coefficients[2,1])
# b.between <- 7.015
# b.within  <- 2.137

HW.corr.ratio*b.between+(1-HW.corr.ratio)*b.within
# b.total = 3.572

summary(HW.out <- lmer(homework ~ 1 + (1|schoolid), NELS))    # empty model
(varcomp.HW <-varcomp(HW.out))       


# arithmetic vs. harmonic.means

nj<-c(30,30,30,30,30)
nj<-c(30,25,35,40,20)
nj<-c(10,20,30,40,50)
nj<-c(5,10,20,50,65)

means(nj)

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

# ICC & residual ICC 
  varcomp(out1)
  varcomp(out5)


# Plausible value range for school means: gamma.hat -/+ t.cv*sqrt(tau)

# Model 1 
fixef(out1)
VarCorr(out1)$schid[1,1]
qt(.975,9)

c(lower.bound <- fixef(out1)-qt(.975,9)*sqrt(VarCorr(out1)$schid[1,1]), upper.bound <- fixef(out1)+qt(.975,9)*sqrt(VarCorr(out1)$schid[1,1]))
upper.bound-lower.bound

# Model 3: HW.groupmean = 0 

fixef(out3)
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
# same as 
out5@ranef

# and the posterior variances are
(postvar <-  attr(re.out5$schid,'postVar')[1,1,])

# These are also the comparative variances.
# The diagnostic variance is calculated using (4.18):
# for model checking (i.e., EB / dig sd  = the standardized empirical Bayes estimators)
(diagvar <- VarCorr(out5)$schid[1,1] - postvar)

# Comparative standard error (a.k.a. posterior standard deviations of U_0j)
# to assess how well the U_0j can be estimated from data to compare levle-2 units with each other
compsd <- sqrt(postvar)

# random coefficient variance tau_00 or tau_0^2 = comp var + diag var

# Bounds of comparative intervals
# compare the equality of level-2 units at the 5 % significance level 
# 1.39 insetad of 1.96 (thus comparative CIs are smaller than CI for assessing single groups)
# See Goldstein & Healy (1995). JRSS, A, 'The graphical representation of a collection of means'.   
lower <- postmean - 1.39*compsd
upper <- postmean + 1.39*compsd

# Order
perm <- order(postmean, lower, upper)
pm_sort <- postmean[perm]
upper_sort <- upper[perm]
lower_sort <- lower[perm]


# A caterpillar plot can be produced as follows.
errbar(1:10, pm_sort, upper_sort, lower_sort)