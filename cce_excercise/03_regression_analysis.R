######################################################################.
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords:
# - general:
######################################################################.

#==============================================#
# ==== load packages and clear objects/log ====
#==============================================#

  # load easimple and clear objects log
  library(easimple)
  ea_start()
  
  # load packages
  library(readxl)
  library(data.table)
  library(eaanalysis)

#====================#
# ==== set parms ====
#====================#

  # set directory
  p_dir <- "C:/Users/Drew/Dropbox/analysis_data/cce_excercise/"

  # define mean function (sig. digits to round to)
  dm_mean <- function(x_var) { round(mean(x_var), 3) }
  
  # output toggle
  p_opt_exp <- 0

#====================#
# ==== load data ====
#====================#

  # load student data file
  in_student_set <- ea_load(paste0(p_dir, "input_data/student_data_format.rdata"))
  
#=========================#
# ==== general format ====
#=========================#
  
  # copy input data
  student_set_working <- copy(in_student_set)
  
  # define list of vars to convert to numeric
  vars_to_numeric <- c("suyi_focus_students", "fallmathrit_13", "sprmathrit_14", "fallreadrit_13", "sprreadrit_14")
  
  # convert necessary vars to numeric
  student_set_working[, vars_to_numeric] <- lapply(student_set_working[, vars_to_numeric, with = FALSE], as.numeric)
  
  # dummy grade and school vars to include in regression
  out_dummy_vars <- db_dummy(in_data            = student_set_working,
                             in_vars_dummy      = c("dm_grade", "schoolname2010"),
                             opt_data_frequency = TRUE)
  
  # copy dummied set
  student_set_working <- copy(out_dummy_vars$out_data_dummy)
  
  # subset to students with non-missing fall and spring rit scores
  regression_set_math <- subset(student_set_working, is.na(fallmathrit_13) == FALSE & is.na(sprmathrit_14) == FALSE)
  regression_set_read <- subset(student_set_working, is.na(fallreadrit_13) == FALSE & is.na(sprreadrit_14) == FALSE)
  
#====================================#
# ==== set regression parameters ====
#====================================#

  # create list of student control vars
  lm_student_controls <- c("d_female_y", "d_dm_hispanic_y", "d_dm_race_black", "d_dm_race_asian", "d_dm_race_amer_indian", "d_sped_y", "d_ell_y", 
                           "d_homeless_y", "suyi_focus_students")
  
  # create list of grade and school vars
  lm_grade_dummies <- grep("d_dm_grade_", colnames(student_set_working), value = TRUE)
  lm_sch_dummies   <- grep("d_schoolname2010_", colnames(student_set_working), value = TRUE)

  # define regression formulas
  lm_math_formula <- paste("sprmathrit_14 ~ fallmathrit_13 +", paste(lm_student_controls, collapse = " + "))
  lm_read_formula <- paste0("sprreadrit_14 ~ fallreadrit_13 +", paste(lm_student_controls, collapse = " + "))

#==========================#
# ==== run regressions ====
#==========================#

  # regression - pooled results
  m1a_math <- lm(lm_math_formula, data = regression_set_math)
  m1b_read <- lm(lm_read_formula, data = regression_set_read)


  
  