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

#====================#
# ==== set parms ====
#====================#

  # store timestamp
  p_timestamp  <- ea_timestamp()
  
  # set project folder (under should be data, documents, results, qc)
  p_dir_root <- paste0("C:/Users/Drew/Dropbox/analysis_data/")
  
  # set output directories
  p_out_dir_recent <- paste0(p_dir_root, "data/most_recent/")
  p_out_dir_date   <- paste0(p_dir_root, "data/by_date/", p_timestamp, "/")
  
  # output toggle
  p_opt_exp <- 0

#====================#
# ==== load data ====
#====================#
  
  # load xwalks - model, test range, demo
  in_model_xwalk <- data.table(read_excel(paste0(p_dir_root, "documents/xwalks/model_xwalk.xlsx")))
  
  # load data
  in_raw_data <- ea_load(paste0(p_dir_root, "data/raw_data_file.rdata"))

  
#=================#
# ==== export ====
#=================#

  # export
  if (p_opt_exp == 1) { 
    
    # create directories
    dir.create(path = p_out_dir_recent, showWarnings = FALSE, recursive = TRUE)
    dir.create(path = p_out_dir_date,   showWarnings = FALSE, recursive = TRUE)

    # export data as rdata file
    save(out_data, file = paste0(p_out_dir_recent, "out_data_file.rdata"), compress = TRUE)
    save(out_data, file = paste0(p_out_dir_date,   "out_data_file.rdata"), compress = TRUE) 
    
    # export data as csv file
    ea_write(out_data, paste0(p_out_dir_recent, "out_data_file.csv"))
    ea_write(out_data, paste0(p_out_dir_date,   "out_data_file.csv"))   
    
  }

