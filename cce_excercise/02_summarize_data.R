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

  # define mean function (sig. digits)
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
  
  # convert necessary vars to numeric
  student_set_working[, c("suyi_focus_students")] <- lapply(student_set_working[, c("suyi_focus_students"), with = FALSE], as.numeric)
  
#=============================================#
# ==== student summarize demographic vars ====
#=============================================#

  # summarize demographic vars - overall
  a_demo_overall <- student_set_working[, list(n_students       = .N,
                                               perc_suyi_focus  = dm_mean(suyi_focus_students),
                                               perc_female      = dm_mean(d_female_y),
                                               perc_hisp        = dm_mean(d_dm_hispanic_y),
                                               perc_white       = dm_mean(d_dm_race_white),
                                               perc_black       = dm_mean(d_dm_race_black),
                                               perc_asian       = dm_mean(d_dm_race_asian),
                                               perc_amer_indian = dm_mean(d_dm_race_amer_indian),
                                               perc_sped        = dm_mean(d_sped_y),
                                               perc_homeless    = dm_mean(d_homeless_y))]
  # summarize demographic vars - by grade
  a_demo_by_grd <- student_set_working[, list(n_students       = .N,
                                              perc_suyi_focus  = dm_mean(suyi_focus_students),
                                              perc_female      = dm_mean(d_female_y),
                                              perc_hisp        = dm_mean(d_dm_hispanic_y),
                                              perc_white       = dm_mean(d_dm_race_white),
                                              perc_black       = dm_mean(d_dm_race_black),
                                              perc_asian       = dm_mean(d_dm_race_asian),
                                              perc_amer_indian = dm_mean(d_dm_race_amer_indian),
                                              perc_sped        = dm_mean(d_sped_y),
                                              perc_homeless    = dm_mean(d_homeless_y)),
                                       by = c("dm_grade")]
  
  # summarize demographic vars - by school
  a_demo_by_sch <- student_set_working[, list(n_students       = .N, 
                                              perc_suyi_focus  = dm_mean(suyi_focus_students),
                                              perc_female      = dm_mean(d_female_y),
                                              perc_hisp        = dm_mean(d_dm_hispanic_y),
                                              perc_white       = dm_mean(d_dm_race_white),
                                              perc_black       = dm_mean(d_dm_race_black),
                                              perc_asian       = dm_mean(d_dm_race_asian),
                                              perc_amer_indian = dm_mean(d_dm_race_amer_indian),
                                              perc_sped        = dm_mean(d_sped_y),
                                              perc_homeless    = dm_mean(d_homeless_y)),
                                       by = c("schoolname2010")]
  

  
  
    
