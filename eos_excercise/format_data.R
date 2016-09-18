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

  # store timestamp
  p_timestamp  <- ea_timestamp()
  
  # set project folder (under should be data, documents, results, qc)
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
  in_raw_data <- data.table(read_excel(paste0(p_dir_root, "data/EOS_3YearDataSummary_HiringAssignment_9_6_16.xlsx"), sheet = 2))

#==========================#
# ==== format raw data ====
#==========================#

  # copy raw data
  eos_summ_data <- copy(in_raw_data)

  # format variable names
  setnames(eos_summ_data, gsub(" ", "_", tolower(colnames(eos_summ_data))))
  
  # replace bad naming conventions
  setnames(eos_summ_data, gsub("#", "num",   colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("%", "perc",  colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("/", "_",     colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("_\\(1,0)", "", colnames(eos_summ_data)))

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

