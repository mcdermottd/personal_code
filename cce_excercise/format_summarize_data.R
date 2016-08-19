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

  # output toggle
  p_opt_exp <- 0

#====================#
# ==== load data ====
#====================#

  # load student data file
  in_raw_data <- data.table(read_excel("C:/Users/Drew/Dropbox/analysis_data/cce_excercise/Mock_SUYI_student_data_F14.xlsx", col_types = rep("text", 33)))

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
    
#===================================#
# ==== convert demographic vars ====
#===================================#
  
  # rename homeless var to dummy
  setnames(student_data_set, c("ishomeless"), c("homeless"))
  
  # create dm race and demo vars
  student_data_set[, ":="(dm_race = tolower(racedesc_p), dm_hispanic = tolower(ethnicitydesc_p))]
  
  # adj. ethnicity values to dummy
  student_data_set[dm_hispanic == "hispanic/latino",     dm_hispanic := "Y"]
  student_data_set[dm_hispanic == "not hispanic/latino", dm_hispanic := "N"]
  
  # adj. / combine race values to dummy
  student_data_set[dm_race == "african american/black", dm_race := "black"]
  student_data_set[chmatch(dm_race, c("asian indian", "cambodian", "chinese", "filipino", "japanese", "korean", "vietnamese", "samoan", "other asian"),
                           nomatch = 0) != FALSE, dm_race := "asian"]
  student_data_set[chmatch(dm_race, c("alaska native", "other american indian"), nomatch = 0) != FALSE, dm_race := "asian"]
    
  # run dummy function
  out_dummy_vars <- db_dummy(in_data            = student_data_set,
                             in_vars_dummy      = c("dm_hispanic", "dm_race", "sped", "homeless"),
                             opt_data_frequency = TRUE)
  
  
#=================#
# ==== export ====
#=================#

  # export
  if (p_opt_exp == 1) { 
    
    ea_write(output_set, ".csv")
    save(return_list, file = ".rdata", compress = TRUE)
  }

