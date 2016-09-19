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
  in_raw_data <- data.table(read_excel(paste0(p_dir_root, "data/raw_data/EOS_3YearDataSummary_HiringAssignment_9_6_16.xlsx"), sheet = 2))

#==========================#
# ==== format raw data ====
#==========================#

  # copy raw data
  eos_summ_data <- copy(in_raw_data)

  # format variable names
  setnames(eos_summ_data, gsub(" ", "_", tolower(colnames(eos_summ_data))))
  
  # replace bad naming conventions
  setnames(eos_summ_data, gsub("#", "num",     colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("%", "perc",    colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("/", "_",       colnames(eos_summ_data)))
  setnames(eos_summ_data, gsub("_\\(1,0)", "", colnames(eos_summ_data)))

#===============================#
# ==== create long data set ====
#===============================#
  
  # create name of measure var columns
  vars_to_melt <-                 grep("closed_",   colnames(eos_summ_data), value = TRUE)
  vars_to_melt <- c(vars_to_melt, grep("underrep",  colnames(eos_summ_data), value = TRUE))
  vars_to_melt <- c(vars_to_melt, grep("benchmark", colnames(eos_summ_data), value = TRUE))
  vars_to_melt <- c(vars_to_melt, grep("total",     colnames(eos_summ_data), value = TRUE))

  # melt measure vars long
  eos_data_long <- melt(eos_summ_data, measure.vars = vars_to_melt, na.rm = TRUE)
  
  # change "variable" from factor to character
  eos_data_long[, variable := as.character(variable)]
  
  # create year variable
  eos_data_long[grepl("13_14", variable), data_yr := 2014]
  eos_data_long[grepl("14_15", variable), data_yr := 2015]
  eos_data_long[grepl("15_16", variable), data_yr := 2016]
  
  # remove year from variable name
  eos_data_long[grepl("13_14", variable), variable := gsub("_13_14", "", variable)]
  eos_data_long[grepl("14_15", variable), variable := gsub("_14_15", "", variable)]
  eos_data_long[grepl("15_16", variable), variable := gsub("_15_16", "", variable)]
  
  # simplify var names
  eos_data_long[variable == "num_underrep_students_added_to_ap_ib_over_baseline", variable := "num_ap_ur_students_added"]
  eos_data_long[variable == "perc_underrep_students_added_to_ap_ib_over_baseline", variable := "perc_ap_ur_students_added"]
  eos_data_long[variable == "num_benchmark_students_added_over_baseline", variable := "num_ap_bm_students_added"]
  eos_data_long[variable == "perc_benchmark_students_added_over_baseline", variable := "perc_ap_bm_students_added"]
  eos_data_long[variable == "total_students_added_to_ap_ib_over_baseline", variable := "num_ap_students_added"]

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

