######################################################################
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords:
# - general:
######################################################################

#######################################
# load packages and clear objects/log #
#######################################

# load easimple and clear objects log
library(easimple)
ea_start()

# load packages
library(foreign)
library(stargazer)
library(data.table)

#############
# set parms #
#############

# output toggle
opt_exp <- 0

#############
# load data #
#############

# load SPSS data file
input_data <- data.table(read.spss("C:/Users/Drew/Dropbox/course_data/nelsb.sav", to.data.frame = TRUE))

#################################
# question 1: total regressions #
#################################

# create copy for analysis
analysis_set <- copy(input_data)

# run total / pooled regression - SES as predictor of math score
m1 <- lm(math ~ ses, data = analysis_set)

# summarize m1
summary(m1)

# run cronbach model - group centered SES (at individual level) and grande centered SES (at group level) as predictors of math score
m2 <- lm(math ~ sesgc + sesmgc, data = analysis_set)

# summarize m2
summary(m2)

###############################################
# question 2: ANOVA regression across schools #
###############################################

# create factor of school number variable for analysis of variance
analysis_set[, schnum_f := as.factor(schnum)]

# calculate an ANOVA model for SES across schools
m3 <- aov(math ~ schnum_f, data = analysis_set)

# summarize m3
summary(m3)

m4 <- aov(math ~ Error(schnum_f), data = analysis_set)

ztest <- lm(math ~ schnum_f, data = analysis_set)
ztest2 <- anova(ztest)



#################################
# output models using stargazer #
#################################

# output models for question 1
stargazer(m1, m2, type="html",
          dep.var.labels = c("Miles/(US) gallon","Fast car (=1)"),
          covariate.labels=c("Gross horsepower","Rear axle ratio","Four foward gears",
                             "Five forward gears","Type of transmission (manual=1)"), out="models.htm")


##########
# export #
##########

# export
if (opt_exp == 1) { 
  
  ea_write( , ".csv")
  
}

