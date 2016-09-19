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
    
#===================================================#
# ==== create additional data sets for plotting ====
#===================================================#

  # copy loaded data
  eos_data <- copy(in_eos_data_long)
  
  # calculate total number of schs that closed gaps, by year
  a_cl_gaps_start_yr <- eos_data[variable == "closed_gaps", list(total_schs  = .N,
                                                                 closed_gaps = sum(value)),
                                 by = c("start_year_with_eos", "data_yr")]
  
  # create variable for schools that didn't close gaps
  a_cl_gaps_start_yr[, not_closed_gaps := closed_gaps - total_schs]
  
  # melt closed and not closed vars long
  cl_gaps_start_yr_long <- melt(a_cl_gaps_start_yr, measure.vars = c("closed_gaps", "not_closed_gaps"))
  
  # sum by data_yr
  cl_gaps_long <- cl_gaps_start_yr_long[, list(total_schs = sum(total_schs),
                                               value      = sum(value)),
                                        by = c("data_yr", "variable")]
  
  # create percentage of schools var
  cl_gaps_start_yr_long[, percent := value / total_schs]
  cl_gaps_long[,          percent := value / total_schs]

#=======================#
# ==== create plots ====
#=======================#
  
  # plot - num schools closed gaps, by year
  plot_gaps_yr <- ggplot(cl_gaps_long, aes(x = data_yr, y = value, fill = variable)) + 
                    geom_bar(stat = "count", position = "identity")
  
  # plot - num schools closed gaps, by year, facetted by start year
  plot_gaps_yr_start_yr <- ggplot(cl_gaps_start_yr_long, aes(x = data_yr, y = value, fill = variable)) + 
                            geom_bar(stat = "identity", position = "identity") + 
                            facet_wrap("start_year_with_eos")
  
  # plot - perc schools closed gaps, by year
  plot_perc_gaps_yr <- ggplot(cl_gaps_long, aes(x = data_yr, y = percent, fill = variable)) + 
                          geom_bar(stat = "identity", position = "identity")
  
  # plot - perc schools closed gaps, by year
  plot_perc_gaps_yr_start_yr <- ggplot(cl_gaps_start_yr_long, aes(x = data_yr, y = percent, fill = variable)) + 
                                  geom_bar(stat = "identity", position = "identity") +
                                  facet_wrap("start_year_with_eos")

           
  
  
  
  
  
  
  
  p <- ggplot(subset(eos_data, variable == "closed_gaps"), aes(x = data_yr)) +  
        geom_bar(aes(y = ..count..))
  
  plot_closed_gaps <- ggplot(data = a_tot_closed_gaps, aes(x = data_yr, y = total_schs)) 

  
  # histogram 
  plot_hist_closed_gaps <- ggplot(data = subset(eos_data, variable == "closed_gaps"), aes(x = data_yr, y = sum(value))) 
  
  
  + 
                                 geom_histogram(binwidth = 15, colour = "black", fill = "dodgerblue4") 

  
  ggplot(data=dat1, aes(x = time, y = total_bill, fill = sex)) +
    geom_bar(stat="identity", position=position_dodge())
  
  # line graph
  
  
  # box and whisker plot - percentage of ap ur students
  
  # scatter plot - percentage of ap ur students above baseline by percentage of bm students above baseline (point for each school)
  
  # does multiply years of EOS participation matter - increase percentage of ap ur students above baseline (is baseline reset every year?)
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

