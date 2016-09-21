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
  # in_eos_data      <- ea_load(paste0(p_dir_root, "data/most_recent/eos_data.rdata"))
  
#============================================================#
# ==== calc schools that sustained gap closing over time ====
#============================================================#

  # copy loaded data
  eos_data <- copy(in_eos_data_long)
  
  # calculate total number of schs that closed gaps, by year
  a_sustain_gap <- eos_data[variable == "sustain_gap", list(total_schs     = .N,
                                                            sustain_gap_cl = sum(value)),
                            by = c("start_year_with_eos", "data_yr")]
  
  # create percentage of schools var
  a_sustain_gap[, perc_sustain_gap_cl := sustain_gap_cl / total_schs]

  # create variable for schools that sustain gap
  a_sustain_gap[, not_sustain_gap := abs(sustain_gap_cl - total_schs)]
  
  # melt closed and not closed vars long
  sustain_gap_long <- melt(a_sustain_gap, measure.vars = c("sustain_gap_cl", "not_sustain_gap"), variable.factor = FALSE)
  
  # set data year to character
  sustain_gap_long[, data_yr := as.character(data_yr)]
  
#===================================================#
# ==== create additional data sets for plotting ====
#===================================================#

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
  
#===========================================#
# ==== create summary tables to display ====
#===========================================#
  
  # add percentage closed gaps variable to closed gaps table
  a_cl_gaps_start_yr[, perc_cl_gaps := round(closed_gaps / total_schs, 2)]
  
  # calc overall avgs
  a_cohort_avgs <- eos_data[is.na(start_year_with_eos) == FALSE, list(total_schs         = .N,
                                                                      avg_yrs_data       = round(mean(num_of_years_of_data), 2),
                                                                      avg_yrs_gp_cl      = round(mean(num_of_years_gaps_closed), 2),
                                                                      avg_num_ur_add_3yr = round(mean(num_of_ur_added_in_3_years), 2)),
                            by = c("start_year_with_eos")]
  
  # take mean of select vars
  a_studs_added_avg <- studs_added_wide[, list(total_schs      = .N,
                                               avg_num_add     = round(mean(num_ap_students_added, na.rm = TRUE), 2),
                                               avg_num_ur_add  = round(mean(num_ap_ur_students_added, na.rm = TRUE), 2),
                                               avg_num_bm_add  = round(mean(num_ap_bm_students_added, na.rm = TRUE), 2),
                                               avg_perc_ur_add = round(mean(perc_ap_ur_students_added, na.rm = TRUE), 2)),
                                      by = c("start_year_with_eos", "data_yr")]
  
  
  # sort data by EOS start year
  setkey(a_cl_gaps_start_yr, start_year_with_eos)
  setkey(a_studs_added_avg, start_year_with_eos)

  # take absolute value of did not close variable to remove negative values
  a_cl_gaps_start_yr[, not_closed_gaps := abs(not_closed_gaps)]
  
#========================================#
# ==== plots - closed gaps (summary) ====
#========================================#
  
  # c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
  
  # plot - num schools closed gaps, by year (Chart 1A)
  plot_gaps_yr <- ggplot(data = cl_gaps_overall, aes(x = data_yr, y = value, fill = variable)) + 
                    geom_bar(stat     = "identity", 
                             position = "identity") +
                    scale_y_continuous(breaks = seq(-100, 100, 25), 
                                       limits = c(-100, 100)) +
                    scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                      labels = c("Closed Gaps", "Did Not Close Gaps")) +
                    theme(legend.title = element_blank()) +
                    labs(x = "Year (FY)", 
                         y = "Number of Schools") +
                    plot_attributes
  
  # plot - perc schools closed gaps, by year (Chart 1B)
  plot_perc_gaps_yr <- ggplot(data = cl_gaps_overall, aes(x = data_yr, y = percent, fill = variable)) + 
                          geom_bar(stat     = "identity", 
                                   position = "identity") +
                          scale_y_continuous(breaks = seq(-1, 1, .25), 
                                             limits = c(-1, 1)) +
                          scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                            labels = c("Closed Gaps", "Did Not Close Gaps")) +
                          theme(legend.title = element_blank()) +
                          labs(x = "Year (FY)", 
                               y = "Percentage of Schools") +
                          plot_attributes
  
  # plot - num schools closed gaps, by year, facetted by start year (Chart 1C)
  plot_gaps_yr_start_yr <- ggplot(data = cl_gaps_start_yr, aes(x = data_yr, y = value, fill = variable)) + 
                            geom_bar(stat     = "identity", 
                                     position = "identity") + 
                            facet_wrap("start_year_with_eos") +
                            # facet_grid(start_year_with_eos ~ .) +
                            # coord_flip() + scale_x_reverse() +
                            scale_y_continuous(breaks = seq(-75, 75, 25), 
                                               limits = c(-75, 75)) +
                            scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                              labels = c("Closed Gaps", "Did Not Close Gaps")) +
                            theme(legend.title = element_blank()) +
                            labs(x = "Year (FY)", 
                                 y = "Number of Schools") +
                            plot_attributes
  
  # plot - perc schools closed gaps, by year, facetted by start year (Chart 1D)
  plot_perc_gaps_yr_start_yr <- ggplot(data = cl_gaps_start_yr, aes(x = data_yr, y = percent, fill = variable)) + 
                                  geom_bar(stat     = "identity", 
                                           position = "identity") +
                                  facet_wrap("start_year_with_eos") +
                                  scale_y_continuous(breaks = seq(-1, 1, .25), 
                                                     limits = c(-1, 1)) +
                                  scale_fill_manual(values = c("#3B9AB2", "#F21A00"), 
                                                    labels = c("Closed Gaps", "Did Not Close Gaps")) +
                                  theme(legend.title = element_blank()) +
                                  labs(x = "Year (FY)", 
                                       y = "Percentage of Schools") +
                                  plot_attributes

#========================================#
# ==== plots - sustained gaps closed ====
#========================================#
  
  # plot
  plot_sustain_gap_cl <- ggplot(subset(sustain_gap_long, variable == "sustain_gap_cl"), aes(start_year_with_eos, value)) +   
                          geom_bar(aes(fill = data_yr), stat     = "identity",
                                                        position = "dodge") + 
                          geom_label(aes(label = paste0(round(perc_sustain_gap_cl*100, 0), " %"), 
                                         group = data_yr),
                                     position = position_dodge(width = 1)) +
                          scale_fill_manual(values = c("#3B9AB2", "#E1AF00"), 
                            labels = c("Yr 1 to Yr 2", "Yr 2 to Yr 3")) +
                          theme(legend.title = element_blank()) +
                          labs(x = "EOS Start Year",
                               y = "Number of Schools that Sustained Gaps Closed") +
                          plot_attributes
  
#===============================================================#
# ==== plots - distribution of percentage of students added ====
#===============================================================#
  
  # plot - box and whisker of percent of ur students added (total students added > 20) (Chart 2A)
  plot_box_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), aes(x = data_yr, y = perc_ap_ur_students_added)) + 
                        geom_boxplot(colour         = "black", 
                                     fill           = "#3B9AB2",
                                     outlier.colour = "#F21A00") +
                        stat_summary(fun.y = mean, 
                                     geom  = "point", 
                                     shape = 5, size = 3) +
                        labs(x = "Year (FY)", 
                             y = "Percent of Underrepresented Students Added to AP/IB Over Baseline") +
                        plot_attributes
  
  # plot - histogram of percent of ur students added (total students added > 20) 
  plot_hist_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), aes(x = perc_ap_ur_students_added)) + 
                          geom_histogram(binwidth = .05,
                                         colour   = "black", 
                                         fill     = "#3B9AB2") +
                          labs(x = "Percent of Underrepresented Students Added to AP/IB Over Baseline", 
                               y = "Number of Schools") +
                          plot_attributes
  
#=============================================#
# ==== plots - school-level scatter plots ====
#=============================================#

  # plot - scatter of schools, students added by perc ur students added (total students added > 20)
  plot_scatter_ur_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), 
                                  aes(x = num_ap_students_added, y = perc_ap_ur_students_added)) + 
                            geom_point(size   = 5,
                                       colour = "#3B9AB2") +
                            labs(x = "Total Number of Students Added to AP/IB Over Baseline", 
                                 y = "Percent of Underrepresented Students Added to AP/IB Over Baseline") +
                            plot_attributes
  
  # # plot - scatter of schools, students added by perc bm students added (total students added > 20)
  # plot_scatter_bm_added <- ggplot(data = subset(studs_added_wide, num_ap_students_added > 20), 
  #                                 aes(x = num_ap_students_added, y = perc_ap_bm_students_added)) + 
  #                           geom_point()
  
  
  # line graph
  
  # does multiply years of EOS participation matter - increase percentage of ap ur students above baseline (is baseline reset every year?)
  
#=================#
# ==== export ====
#=================#
  
  # define output
  out_list <- list(eos_data_long           = eos_data,
                      eos_data_st_added    = studs_added_wide,
                      tb_cohort_avgs       = a_cohort_avgs,
                      tb_cl_gaps           = cl_gaps_overall,
                      tb_cl_gaps_srt_yr    = cl_gaps_start_yr,
                      tb_cl_gaps_srt_yr_wd = a_cl_gaps_start_yr,
                      tb_sustain_gap_cl    = sustain_gap_long,
                      tb_st_added          = a_studs_added_avg)
  
  # export
  if (p_opt_exp == 1) { 
    
    # export data as rdata file
    save(out_list, file = paste0(p_out_dir_recent, "flex_dash_data.rdata"), compress = TRUE)
    save(out_list, file = "C:/Users/Drew/Dropbox/github_clones/personal_code/eos_excercise/flex_dash_data.rdata", compress = TRUE)
  
  }

