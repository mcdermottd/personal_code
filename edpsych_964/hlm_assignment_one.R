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
library(dplyr)
library(broom)

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
m3 <- aov(lm(ses ~ schnum_f, data = analysis_set))

test = aov(math ~ Error(schnum_f), data = analysis_set)
summary(test)

m3_tidy <- tidy(m3)

##############################################
# question 3: between and within-regressions #
##############################################

# take mean of ses and math score by school
school_means <- analysis_set[, list(ses_mean = mean(ses),
                                    math_mean = mean(math)), by = schnum]

# run between groups regression - SES as predictor of math score at school level
m4 <- lm(math_mean ~ ses_mean, data = school_means)

# summarize m4
summary(m4)

# run within groups regression - average school math score and group-centered SES as predictor of math score at individual level
m5 <- lm(math ~ math_1 + sesgc, data = analysis_set)

# summarize m5
summary(m5)

####################################################
# question 4: seperate regressions for each school #
####################################################

# break up analysis_set by school - run regression with SES as predictor of math score for each school
school_models <- analysis_set %>% group_by(schnum) %>% do(tidy(lm(math ~  ses, data = .)))

#################################
# output models using stargazer #
#################################

if (opt_exp == 1) { 
  
  # output models for question 1
  stargazer(m1, m2, type = "html",
            dep.var.labels = "Student Math Score",
            covariate.labels = c("Student SES", "SES (Group-Centered)", "SES (Grand-Centered)"),
            report = "vc*s",
            out = "C:/Users/Drew/Dropbox/course_data/question1_models.htm")
  
  # output table of ANOVA results for question 2
  ea_write(m3_tidy, "C:/Users/Drew/Dropbox/course_data/question2_anova.csv")
  
  
  # output models for question 3
  stargazer(m1, m4, m5, type = "html",
            dep.var.labels = c("Student Math Score", "School Mean Math Score", "Student Math Score"),
            covariate.labels = c("Student SES", "School Mean SES", "School Mean Math Score", "SES (Group-Centered)"),
            report = "vc*s",
            out = "C:/Users/Drew/Dropbox/course_data/question3_models.htm")
  
  # output table of school-level models for question 4
  ea_write(school_models, "C:/Users/Drew/Dropbox/course_data/question4_models.csv")
  
}




