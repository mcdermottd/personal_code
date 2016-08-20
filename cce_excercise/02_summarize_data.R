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
  in_student_set <- ea_load(paste0(p_dir, "input_data/student_data_format.rdata"))
  
  
  
  
  
