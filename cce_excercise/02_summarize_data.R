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
  
#=====================================#
# ==== summarize demographic vars ====
#=====================================#

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
  a_demo_grd <- student_set_working[, list(n_students       = .N,
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
  
  # summarize demographic vars - by sch type
  a_demo_sch_type <- student_set_working[, list(n_students       = .N, 
                                                perc_suyi_focus  = dm_mean(suyi_focus_students),
                                                perc_female      = dm_mean(d_female_y),
                                                perc_hisp        = dm_mean(d_dm_hispanic_y),
                                                perc_white       = dm_mean(d_dm_race_white),
                                                perc_black       = dm_mean(d_dm_race_black),
                                                perc_asian       = dm_mean(d_dm_race_asian),
                                                perc_amer_indian = dm_mean(d_dm_race_amer_indian),
                                                perc_sped        = dm_mean(d_sped_y),
                                                perc_homeless    = dm_mean(d_homeless_y)),
                                         by = c("sch_type")]
  
  # summarize demographic vars - by school
  a_demo_sch <- student_set_working[, list(n_students       = .N, 
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
  
#===============================================#
# ==== melt academic vars long to summarize ====
#===============================================#
  
  # set demo vars
  demo_vars <- c("sex", "dm_race", "dm_hispanic", "sped", "homeless", "suyi_focus_students", "sch_type", "dm_grade", "schoolname2010")
  acad_vars <- c("gpa_14", "daysenrolled", "daysabsent", "daysunexcused", "attendpercent", "fallmathrit_13", "wtrmathrit_14", "sprmathrit_14",
                 "fallreadrit_13", "wtrreadrit_14", "sprreadrit_14", "z_msp_hspemathscore_14", "z_msp_hspereadscore_14")
  
  # subset to data for melt
  student_set_melt <- subset(student_set_working, select = c("dm_student_id", demo_vars, acad_vars)) 
  
  # convert all acad vars to numeric for melt
  student_set_melt[, acad_vars] <- lapply(student_set_melt[, acad_vars, with = FALSE], as.numeric)
  
  # melt data long to summarize
  student_data_long <- melt(student_set_melt, id.vars = c("dm_student_id", demo_vars))
                              
  # remove NA values
  student_data_long <- subset(student_data_long, !is.na(value))
  
#===============================#
# ==== summarize acad. vars ====
#===============================#
  
  # summarize acad vars - overall
  a_acad_overall <- student_data_long[, list(n_obs = length(value),
                                             min = min(value),
                                             q25 = quantile(value, .25),
                                             q50 = quantile(value, .5),
                                             q75 = quantile(value, .75),
                                             max = max(value),
                                             mean = dm_mean(value),
                                             var = round(var(value), 3),
                                             sd = round(sd(value), 3)), 
                                      by = variable]

  # summarize acad vars - by school type
  a_acad_sch_type <- student_data_long[, list(n_obs = length(value),
                                              min = min(value),
                                              q25 = quantile(value, .25),
                                              q50 = quantile(value, .5),
                                              q75 = quantile(value, .75),
                                              max = max(value),
                                              mean = dm_mean(value),
                                              var = round(var(value), 3),
                                              sd = round(sd(value), 3)), 
                                       by = c("variable", "sch_type")]
  
  # summarize acad vars - by grade
  a_acad_grd <- student_data_long[, list(n_obs = length(value),
                                         min = min(value),
                                         q25 = quantile(value, .25),
                                         q50 = quantile(value, .5),
                                         q75 = quantile(value, .75),
                                         max = max(value),
                                         mean = dm_mean(value),
                                         var = round(var(value), 3),
                                         sd = round(sd(value), 3)), 
                                  by = c("variable", "sch_type")]
  
#=================#
# ==== export ====
#=================#

  # copy data to export
  out_student_data <- copy(student_data_zscore)
  
  # export
  if (p_opt_exp == 1) { 
    
    # output demo summary tables
    ea_write(a_demo_overall,  paste0(p_dir, "qc/summ_demo_overall.csv"))
    ea_write(a_demo_grd,      paste0(p_dir, "qc/summ_demo_grd.csv"))
    ea_write(a_demo_sch_type, paste0(p_dir, "qc/summ_demo_sch_type.csv"))
    ea_write(a_demo_sch,      paste0(p_dir, "qc/summ_demo_sch.csv"))
    
  }  
    

  
  
  
  
  
  