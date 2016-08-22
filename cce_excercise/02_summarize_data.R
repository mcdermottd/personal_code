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
  
  # convert necessary vars to numeric
  student_set_working[, c("suyi_focus_students")] <- lapply(student_set_working[, c("suyi_focus_students"), with = FALSE], as.numeric)

#===========================================#
# ==== summarize school characteristics ====
#===========================================#
  
  # take frequency of students by school
  a_freq_sch <- ea_table(student_set_working, c("schoolname2010"), opt_percent = 1)

  # summarize students per school
  a_sch_students <- a_freq_sch[, list(n_schools    = .N,
                                      avg_students = dm_mean(count),
                                      med_students = quantile(count, .5),
                                      min_students = min(count),
                                      max_students = max(count))]
  
  # take frequency of students by school and type
  a_freq_sch_type <- ea_table(student_set_working, c("sch_type", "schoolname2010"))
  
  # summarize students per school type
  a_sch_students_type <- a_freq_sch_type[, list(n_schools    = .N,
                                                avg_students = dm_mean(count),
                                                med_students = quantile(count, .5),
                                                min_students = min(count),
                                                max_students = max(count)),
                                         by = "sch_type"]
                                        
  # add vars for stacking
  a_sch_students[,      type := "overall"]
  a_sch_students_type[, type := "sch_type"]

  # stack together
  stacked_sch_cts <- rbind(a_sch_students, a_sch_students_type, fill = TRUE)

  # reorder vars
  ea_colorder(stacked_sch_cts, c("type", "sch_type"))
  
  #remove: sets needed for summaries
  rm(a_freq_sch, a_sch_students, a_freq_sch_type, a_sch_students_type)
  
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
  
  # add vars for stacking
  a_demo_overall[,  type := "overall"]
  a_demo_sch_type[, type := "sch_type"]
  a_demo_grd[,      type := "grade"]
  a_demo_sch[,      type := "school"]

  # stack together
  stacked_demo_stats <- rbind(a_demo_overall,     a_demo_sch_type, fill = TRUE)
  stacked_demo_stats <- rbind(stacked_demo_stats, a_demo_grd, fill = TRUE)
  stacked_demo_stats <- rbind(stacked_demo_stats, a_demo_sch, fill = TRUE)
    
  # reorder vars
  ea_colorder(stacked_demo_stats, c("type", "sch_type", "dm_grade", "schoolname2010"))
  
  #remove: individual sets
  rm(a_demo_overall, a_demo_sch_type, a_demo_grd, a_demo_sch)
  
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
                                             mean  = dm_mean(value),
                                             med   = quantile(value, .5),
                                             var   = round(var(value), 3),
                                             sd    = round(sd(value), 3),
                                             min   = min(value),
                                             q25   = quantile(value, .25),
                                             q75   = quantile(value, .75),
                                             max   = max(value)), 
                                      by = variable]

  # summarize acad vars - by school type
  a_acad_sch_type <- student_data_long[, list(n_obs = length(value),
                                              mean  = dm_mean(value),
                                              med   = quantile(value, .5),
                                              var   = round(var(value), 3),
                                              sd    = round(sd(value), 3),
                                              min   = min(value),
                                              q25   = quantile(value, .25),
                                              q75   = quantile(value, .75),
                                              max   = max(value)), 
                                       by = c("variable", "sch_type")]
  
  # summarize acad vars - by grade
  a_acad_grd <- student_data_long[, list(n_obs = length(value),
                                         mean  = dm_mean(value),
                                         med   = quantile(value, .5),
                                         var   = round(var(value), 3),
                                         sd    = round(sd(value), 3),
                                         min   = min(value),
                                         q25   = quantile(value, .25),
                                         q75   = quantile(value, .75),
                                         max   = max(value)), 
                                  by = c("variable", "dm_grade")]
  
  # add vars for stacking
  a_acad_overall[,  type := "overall"]
  a_acad_sch_type[, type := "sch_type"]
  a_acad_grd[,      type := "grade"]

  # stack together
  stacked_acad_stats <- rbind(a_acad_overall,     a_acad_sch_type, fill = TRUE)
  stacked_acad_stats <- rbind(stacked_acad_stats, a_acad_grd, fill = TRUE)
  
  # reorder vars
  ea_colorder(stacked_acad_stats, c("type", "sch_type", "dm_grade"))
  
  #remove: individual sets
  rm(student_set_melt, a_acad_overall, a_acad_sch_type, a_acad_grd)
  
#============================================#
# ==== summarize acad. vars by subgroups ====
#============================================#

  # summarize acad vars - by suyi focus student status
  a_acad_suyi <- student_data_long[, list(n_obs = length(value),
                                          mean  = dm_mean(value),
                                          med   = quantile(value, .5),
                                          var   = round(var(value), 3),
                                          sd    = round(sd(value), 3),
                                          min   = min(value),
                                          q25   = quantile(value, .25),
                                          q50   = quantile(value, .5),
                                          q75   = quantile(value, .75),
                                          max   = max(value)), 
                                   by = c("variable", "suyi_focus_students")]
  
  # summarize acad vars - by gender
  a_acad_gender <- student_data_long[, list(n_obs = length(value),
                                            mean  = dm_mean(value),
                                            med   = quantile(value, .5),
                                            var   = round(var(value), 3),
                                            sd    = round(sd(value), 3),
                                            min   = min(value),
                                            q25   = quantile(value, .25),
                                            q50   = quantile(value, .5),
                                            q75   = quantile(value, .75),
                                            max   = max(value)), 
                                   by = c("variable", "sex")]
  
  # summarize acad vars - by race
  a_acad_race <- student_data_long[, list(n_obs = length(value),
                                          mean  = dm_mean(value),
                                          med   = quantile(value, .5),
                                          var   = round(var(value), 3),
                                          sd    = round(sd(value), 3),
                                          min   = min(value),
                                          q25   = quantile(value, .25),
                                          q50   = quantile(value, .5),
                                          q75   = quantile(value, .75),
                                          max   = max(value)),  
                                   by = c("variable", "dm_race")]
  
  # add vars for stacking
  a_acad_suyi[,   type := "suyi_status"]
  a_acad_gender[, type := "gender"]
  a_acad_race[,   type := "race"]

  # stack together
  stacked_acad_stats_sg <- rbind(a_acad_suyi,           a_acad_gender, fill = TRUE)
  stacked_acad_stats_sg <- rbind(stacked_acad_stats_sg, a_acad_race, fill = TRUE)
  
  # reorder vars
  ea_colorder(stacked_acad_stats_sg, c("type", "sex", "dm_race"))
  
  #remove: individual sets
  rm(student_data_long, a_acad_suyi, a_acad_gender, a_acad_race)
  
#=================#
# ==== export ====
#=================#

  # export
  if (p_opt_exp == 1) { 
    
    # output stacked sets
    ea_write(stacked_sch_cts,       paste0(p_dir, "qc/summ_sch_counts.csv"))
    ea_write(stacked_demo_stats,    paste0(p_dir, "qc/summ_stats_demo.csv"))
    ea_write(stacked_acad_stats,    paste0(p_dir, "qc/summ_stats_acad.csv"))
    ea_write(stacked_acad_stats_sg, paste0(p_dir, "qc/summ_stats_acad_subgroup.csv"))
    
  }  
    

  