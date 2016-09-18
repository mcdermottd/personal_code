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
  library(ggplot2)
  library(data.table)

#====================#
# ==== set parms ====
#====================#

  # store timestamp
  p_timestamp  <- ea_timestamp()
  
  # set project folder
  p_dir_root <- "C:/Users/Drew/Dropbox/analysis_data/eos_excercise/"
  
  # set output directories
  p_out_dir_recent <- paste0(p_dir_root, "data/most_recent/")
  p_out_dir_date   <- paste0(p_dir_root, "data/by_date/", p_timestamp, "/")
  
  # output toggle
  p_opt_exp <- 0

#====================#
# ==== load data ====
#====================#
  
  # load data
  in_eos_data_long <- ea_load(paste0(p_dir_root, "data/most_recent/eos_data_long.rdata"))
    


#=================#
# ==== export ====
#=================#

  # copy long file to export
  out_data_long <- copy(eos_data_long)
  
  # export
  if (p_opt_exp == 1) { 
    
    # create directories
    dir.create(path = p_out_dir_recent, showWarnings = FALSE, recursive = TRUE)
    dir.create(path = p_out_dir_date,   showWarnings = FALSE, recursive = TRUE)

    # export data as rdata file
    save(out_data_long, file = paste0(p_out_dir_recent, "eos_data_long.rdata"), compress = TRUE)
    save(out_data_long, file = paste0(p_out_dir_date,   "eos_data_long.rdata"), compress = TRUE) 
    
    # export data as csv file
    ea_write(out_data_long, paste0(p_out_dir_recent, "eos_data_long.csv"))
    ea_write(out_data_long, paste0(p_out_dir_date,   "eos_data_long.csv"))   
    
  }

