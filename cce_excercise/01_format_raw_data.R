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

  # output toggle
  p_opt_exp <- 0

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
  
  # create grade var
  student_data_set[, dm_grade := grade]
  student_data_set[dm_grade == "K", dm_grade := "0"]
  student_data_set[nchar(dm_grade) == 1, dm_grade := paste0("0", dm_grade)]
  
  # create model descr var from grade
  student_data_set[, model_descr := paste0("dm_", dm_grade)]
    
#===================================#
# ==== convert demographic vars ====
#===================================#
  
  # rename homeless var to dummy
  setnames(student_data_set, c("ishomeless"), c("homeless"))
  
  # convert sped and homeless values to lowercase
  student_data_set[, ":="(sped = tolower(sped), homeless = tolower(homeless))]
  
  # create dm race and demo vars
  student_data_set[, ":="(dm_race = tolower(racedesc_p), dm_hispanic = tolower(ethnicitydesc_p))]
  
  # adj. ethnicity values to dummy
  student_data_set[dm_hispanic == "hispanic/latino",     dm_hispanic := "y"]
  student_data_set[dm_hispanic == "not hispanic/latino", dm_hispanic := "n"]
  
  # adj. / combine race values to dummy
  student_data_set[dm_race == "african american/black", dm_race := "black"]
  student_data_set[chmatch(dm_race, c("asian indian", "cambodian", "chinese", "filipino", "japanese", "korean", "vietnamese", "samoan", "other asian"),
                           nomatch = 0) != FALSE, dm_race := "asian"]
  student_data_set[chmatch(dm_race, c("alaska native", "other american indian"), nomatch = 0) != FALSE, dm_race := "asian"]
    
  # run dummy function
  out_dummy_vars <- db_dummy(in_data            = student_data_set,
                             in_vars_dummy      = c("dm_hispanic", "dm_race", "sped", "homeless"),
                             opt_data_frequency = TRUE)
  
  # copy dummy function output
  student_data_dummy <- copy(out_dummy_vars$out_data_dummy)
  
  #remove: non-dummied set
  rm(student_data_set)
  
#========================================#
# ==== convert test score to z-units ====
#========================================#
  
  # set scores to z
  tests_to_z <- c("2013fallmathrit", "2014wtrmathrit", "2014sprmathrit", "2013fallreadrit", "2014wtrreadrit", "2014sprreadrit")
  
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
  rm(student_data_dummy)
  
#=================#
# ==== export ====
#=================#

  # copy data to export
  out_student_data <- copy(student_data_zscore)
  
  # export
  if (p_opt_exp == 1) { 
    
    ea_write(out_student_data, paste0(p_dir, "input_data/student_data_format.csv"))
    save(out_student_data, file = paste0(p_dir, "input_data/student_data_format.rdata"), compress = TRUE)
  }

