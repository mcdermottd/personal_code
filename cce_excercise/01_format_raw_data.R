######################################################################.
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords: #brule #check
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

  # output toggle
  p_opt_exp <- 1

#====================#
# ==== load data ====
#====================#

  # load student data file
  in_raw_data <- data.table(read_excel(paste0(p_dir, "raw_data/Mock_SUYI_student_data_F14.xlsx"), col_types = rep("text", 33)))

#==========================#
# ==== format raw data ====
#==========================#
  
  # copy raw file
  student_data_set <- copy(in_raw_data)

  # change variable names to lowercase
  setnames(student_data_set, tolower(colnames(student_data_set)))

  # replace spaces and "/" with "_" in variable names
  setnames(student_data_set, colnames(student_data_set), gsub(" ", "_", colnames(student_data_set)))
  setnames(student_data_set, colnames(student_data_set), gsub("/", "_", colnames(student_data_set)))
  
  # create student id var
  student_data_set[, dm_student_id := paste0("dm_", rownames(student_data_set))]
  
  # identify vars with year in name
  yr_vars_13 <- grep("2013", colnames(student_data_set), value = TRUE)
  yr_vars_14 <- grep("2014", colnames(student_data_set), value = TRUE)

  # rename year vars
  setnames(student_data_set, yr_vars_13, paste0(gsub("2013", "", yr_vars_13), "_13"))
  setnames(student_data_set, yr_vars_14, paste0(gsub("2014", "", yr_vars_14), "_14"))

#==================================#
# ==== format demographic vars ====
#==================================#
  
  # rename homeless var to dummy
  setnames(student_data_set, c("ishomeless"), c("homeless"))
  
  # convert sped and homeless values to lowercase
  student_data_set[, ":="(sped = tolower(sped), homeless = tolower(homeless))]
  
  # create dm race and demo vars
  student_data_set[, ":="(dm_race = tolower(racedesc_p), dm_hispanic = tolower(ethnicitydesc_p))]
  
  # adj. ethnicity values to dummy
  student_data_set[dm_hispanic == "hispanic/latino",     dm_hispanic := "y"]
  student_data_set[dm_hispanic == "not hispanic/latino", dm_hispanic := "n"]
  
  # create gender vars to dummy
  student_data_set[, female := ifelse(sex == "F", "y", "n")]
  student_data_set[, male   := ifelse(sex == "M", "y", "n")]

  # adj. / combine race values to dummy #brule
  student_data_set[dm_race == "african american/black", dm_race := "black"]
  student_data_set[chmatch(dm_race, c("asian indian", "cambodian", "chinese", "filipino", "japanese", "korean", "vietnamese", "samoan", "other asian"),
                           nomatch = 0) != FALSE, dm_race := "asian"]
  student_data_set[chmatch(dm_race, c("alaska native", "other american indian"), nomatch = 0) != FALSE, dm_race := "amer_indian"]

  # create non-english speaking vars
  student_data_set[, plang_non_engl := ifelse(is.na(primarylanguage) | primarylanguage == "English", 0, 1)]
  student_data_set[, hlang_non_engl := ifelse(is.na(homelanguage) | homelanguage == "English", 0, 1)]
  
#============================================#
# ==== create grade and school type vars ====
#============================================#

  # create grade var (set K to grade 0)
  student_data_set[, dm_grade := grade]
  student_data_set[dm_grade == "K", dm_grade := "0"]
  
  # convert to numeric
  student_data_set[, dm_grade := as.numeric(dm_grade)]
  
  # create grade type vars #brule
  student_data_set[dm_grade < 3, sch_type := "ps"]
  student_data_set[dm_grade > 2 & dm_grade < 9, sch_type := "es_ms"]
  student_data_set[dm_grade > 8, sch_type := "hs"]
  
  # change grade back to character and format
  student_data_set[, dm_grade := as.character(dm_grade)]
  student_data_set[nchar(dm_grade) == 1, dm_grade := paste0("0", dm_grade)]
  
  # create model descr var from grade
  student_data_set[, model_descr := paste0("dm_", dm_grade)]
  
#=================================#
# ==== format assessment vars ====
#=================================#
  
  # create pl level vars
  student_data_set[, ":="(dm_pl_math = msp_hspemathlevel_14, dm_pl_read = msp_hspereadlevel_14)]

  # change missings to NA #brule
  student_data_set[dm_pl_math == "--", dm_pl_math := NA]
  student_data_set[dm_pl_read == "--", dm_pl_read := NA]
  
  # change "MO" to missing #brule #check
  student_data_set[dm_pl_math == "MO", dm_pl_math := NA]
  student_data_set[dm_pl_read == "MO", dm_pl_read := NA]
  
  # if relevant score is missing, set PL level to NA #brule
  student_data_set[is.na(msp_hspemathscore_14), dm_pl_math := NA]
  student_data_set[is.na(msp_hspereadscore_14), dm_pl_read := NA]
  
  # set GPA var of 0 to missing #brule
  student_data_set[gpa_14 == "0", gpa_14 := NA]
  
#==============================#
# ==== dummy relevant vars ====
#==============================#

  # run dummy function
  out_dummy_vars <- db_dummy(in_data            = student_data_set,
                             in_vars_dummy      = c("male", "female", "dm_hispanic", "dm_race", "sped", "homeless", "dm_pl_math", "dm_pl_read"),
                             opt_data_frequency = TRUE)
  
  # copy dummy function output
  student_data_dummy <- copy(out_dummy_vars$out_data_dummy)
  
  # change missing back to NA #check db_dummy function issue
  student_data_dummy[dm_pl_math == "missing", dm_pl_math := NA]
  student_data_dummy[dm_pl_read == "missing", dm_pl_read := NA]
  
  #remove: non-dummied set
  rm(student_data_set)
  
#=========================================#
# ==== convert test scores to z-units ====
#=========================================#
  
  # set scores to z
  tests_to_z <- c("msp_hspemathscore_14", "msp_hspereadscore_14")
  
  # convert test score vars to numeric
  student_data_dummy[, tests_to_z] <- lapply(student_data_dummy[, tests_to_z, with = FALSE], as.numeric)
  
  # initialize list for loop
  out_zscore_list <- list()
  
  # start loop to z-score by grade
  for (m_model in unique(student_data_dummy$model_descr)) {

    # subset student data to model
    zscore_set <- subset(student_data_dummy, model_descr == m_model)
  
    # zscore grade specific set
    out_zscore_vars <- db_zscore(in_data_student_scores       = zscore_set,
                                 in_var_student_id            = "dm_student_id",
                                 in_vars_scores_without_sems  = tests_to_z,
                                 in_vars_scores_with_sems     = NULL,
                                 in_vars_sems                 = NULL,
                                 in_val_model_descr           = m_model,
                                 in_val_prefix_normalz        = "z_",
                                 in_val_prefix_rankz          = "rankz_",
                                 opt_val_z_descr              = "",
                                 opt_include_normalz          = TRUE,
                                 opt_include_rankz            = FALSE)
    
    # add output to list
    out_zscore_list[[m_model]] <- out_zscore_vars
    
  }
  
  # extract and stack data sets
  out_stacked_zcore <- ea_extract(out_zscore_list)
  
  # copy zscore function output
  student_data_zscore <- copy(out_stacked_zcore$out_data_zscore)
  
  #remove: dummy set
  rm(student_data_dummy, zscore_set, out_zscore_list)
  
#=======================#
# ==== final format ====
#=======================#
  
  # reorder vars
  ea_colorder(student_data_zscore, c("model_descr", "dm_student_id", "sex", "dm_race", "dm_hispanic", "sped", "homeless", "suyi_focus_students", 
                                     "plang_non_engl", "sch_type", "dm_grade", "schoolname2010"))
  
#=================#
# ==== export ====
#=================#

  # copy data to export
  out_student_data <- copy(student_data_zscore)
  
  # export
  if (p_opt_exp == 1) { 
    
    # output formatted data sets
    ea_write(out_student_data, paste0(p_dir, "input_data/student_data_format.csv"))
    save(out_student_data, file = paste0(p_dir, "input_data/student_data_format.rdata"), compress = TRUE)
    
    # output dummy and zscore stats
    ea_write(out_dummy_vars$out_dummy_freqs, paste0(p_dir, "qc/freq_dummy_vars.csv"))
    ea_write(out_stacked_zcore$out_data_zparms, paste0(p_dir, "qc/zscore_parms.csv"))
    
  }

