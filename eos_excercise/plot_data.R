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
  library(wesanderson)
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
# ==== set parms ====
#====================#

  # set wes anderson color scheme
  wes_pal <- wes_palette(name = "Zissou", type = "continuous")

  # set up base plot attributes / theme 
  plot_attributes <- theme(plot.background    = element_rect(fill = "lightgrey"),
                           panel.grid.major.x = element_line(color = "gray90"), 
                           panel.grid.minor   = element_blank(),
                           panel.background   = element_rect(fill = "white", colour = "black") , 
                           panel.grid.major.y = element_line(color = "gray90"),
                           text               = element_text(size = 20),
                           plot.title         = element_text(vjust = 0, colour = "black", face = "bold", size = 25))
  
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
  cl_gaps_start_yr <- melt(a_cl_gaps_start_yr, measure.vars = c("closed_gaps", "not_closed_gaps"), variable.factor = FALSE)
  
  # sum by data_yr
  cl_gaps_overall <- cl_gaps_start_yr[, list(total_schs = sum(total_schs),
                                             value      = sum(value)),
                                      by = c("data_yr", "variable")]
  
  # create percentage of schools var
  cl_gaps_start_yr[, percent := value / total_schs]
  cl_gaps_overall[,  percent := value / total_schs]
  
  # cast students added variables wide, by year
  studs_added_wide <- dcast(subset(eos_data, variable != "closed_gaps"), district_id + school_id + num_of_years_of_data + start_year_with_eos + 
                              num_of_years_gaps_closed + num_of_ur_added_in_3_years + num_of_ur_added_across_district + data_yr ~ variable, 
                              value.var = c("value"))

  # set data year to character
  studs_added_wide[, data_yr := as.character(data_yr)]
  
#========================================#
# ==== plots - closed gaps (summary) ====
#========================================#
  
  # c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
  
  # plot - num schools closed gaps, by year
  plot_gaps_yr <- ggplot(data = cl_gaps_overall, aes(x = data_yr, y = value, fill = variable)) + 
                    geom_bar(stat = "identity", position = "identity") +
                    scale_y_continuous(breaks = seq(-100, 100, 25), limits = c(-100, 100)) +
                    scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                      labels = c("Closed Gaps", "Did Not Close Gaps")) +
                    theme(legend.title = element_blank()) +
                    labs(x = "Year (FY)", y = "Number of Schools") +
                    plot_attributes
  
  # plot - num schools closed gaps, by year, facetted by start year
  plot_gaps_yr_start_yr <- ggplot(data = cl_gaps_start_yr, aes(x = data_yr, y = value, fill = variable)) + 
                            geom_bar(stat = "identity", position = "identity") + 
                            facet_wrap("start_year_with_eos") +
                            scale_y_continuous(breaks = seq(-75, 75, 25), limits = c(-75, 75)) +
                            scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                              labels = c("Closed Gaps", "Did Not Close Gaps")) +
                            theme(legend.title = element_blank()) +
                            labs(x = "Year (FY)", y = "Number of Schools") +
                            plot_attributes
  
  # plot - perc schools closed gaps, by year
  plot_perc_gaps_yr <- ggplot(data = cl_gaps_overall, aes(x = data_yr, y = percent, fill = variable)) + 
                          geom_bar(stat = "identity", position = "identity") +
                          scale_y_continuous(breaks = seq(-1, 1, .25), limits = c(-1, 1)) +
                          scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                            labels = c("Closed Gaps", "Did Not Close Gaps")) +
                          theme(legend.title = element_blank()) +
                          labs(x = "Year (FY)", y = "Number of Schools") +
                          plot_attributes
  
  # plot - perc schools closed gaps, by year, facetted by start year
  plot_perc_gaps_yr_start_yr <- ggplot(data = cl_gaps_start_yr, aes(x = data_yr, y = percent, fill = variable)) + 
                                  geom_bar(stat = "identity", position = "identity") +
                                  facet_wrap("start_year_with_eos") +
                                  scale_y_continuous(breaks = seq(-1, 1, .25), limits = c(-1, 1)) +
                                  scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                                    labels = c("Closed Gaps", "Did Not Close Gaps")) +
                                  theme(legend.title = element_blank()) +
                                  labs(x = "Year (FY)", y = "Number of Schools") +
                                  plot_attributes

#===============================================================#
# ==== plots - distribution of percentage of students added ====
#===============================================================#
  
  # plot - histogram of percent of ur students added (total students added > 20) 
  plot_hist_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), aes(x = perc_ap_ur_students_added)) + 
                          geom_histogram() 
  
  # plot - box and whisker of percent of ur students added (total students added > 20) 
  plot_box_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), aes(x = data_yr, y = perc_ap_ur_students_added)) + 
                        geom_boxplot() +
                        stat_summary(fun.y = mean, geom = "point", shape = 5, size = 3)
  
#=============================================#
# ==== plots - school-level scatter plots ====
#=============================================#

  # plot - scatter of schools, students added by perc ur students added (total students added > 20)
  plot_scatter_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), 
                                  aes(x = num_ap_students_added, y = perc_ap_ur_students_added)) + 
                            geom_point()
  
  # plot - scatter of schools, students added by perc bm students added (total students added > 20)
  plot_scatter_bm_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), 
                                  aes(x = num_ap_students_added, y = perc_ap_bm_students_added)) + 
                            geom_point()
  
  
  # line graph
  
  
  # box and whisker plot - percentage of ap ur students
  
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

