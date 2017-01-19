
#===================#
# ==== packages ====
#===================#

  # start Rserve session
  library(Rserve)
  Rserve()
  
  # install dev version of apsrtable
  # install_github("malecki/apsrtable", subdir = "pkg")
  devtools::install_github("jknowles/apsrtable", subdir = "pkg")
  
  # list functions in package
  ls(pos = "package:easimple")

  # check code speed line by line (add code to profile inside profvis)
  time <- profvis({ })

#==========================#
# ==== basic functions ====
#==========================#

  # view certain columns in data.table (paste 100 - 2 options)
  View(data.frame(set_name)[, 50:ncol(set_name)])
  View(set_name[, 50:ncol(set_name), with = FALSE])
     
  # return name of object
  deparse(substitute(object))
  
  # use the quote() and eval() functions to pass a variable to data.table
  temp <- quote(x)
  DT[, eval(temp)]
  
  # function to assign value to name, with var name as character string
  assign(var_name, NULL)

#============================#
# ==== data manipulation ====
#============================#
  
  # change multiple variables to numeric (using lapply)
  set_name[, 2:ncol(set_name)] <- lapply(set_name[, 2:ncol(set_name), with = FALSE], as.numeric)
  
  # set multiple variables at once
  set_name[, ":="(model_descr = descr, dummy_id = NULL)]
  
  # data.table dcast to cast multiple variables
  wide_set <- data.table::dcast(set_name, subject + grade + school_type + level_id ~ model_group, value.var = c("level_n", "est", "pnormed"))
  
  # number groupings to cast wide
  long_set[, count := seq_len(.N), by = id_var]

#============================#
# ==== data calculations ====
#============================#

  # lubridate: calculate duration between two dates (returns in seconds so divide by 86400)
  set_name[, duration := as.numeric(as.duration(ymd(end_date) - ymd(start_date))) / 86400]
  
  # calc summary stats on long file, with by group
  a_summary_stats <- dt_long[is.na(value) == FALSE, list(n_obs = length(value),
                                                         min   = min(value),
                                                         q25   = quantile(value, .25),
                                                         q50   = quantile(value, .5),
                                                         q75   = quantile(value, .75),
                                                         max   = max(value),
                                                         mean  = round(mean(value), 2),
                                                         var   = round(var(value), 2),
                                                         sd    = round(sd(value), 2)),
                             by = c("var1", "var2")]

#=========================#
# ==== visualizations ====
#=========================#

  # EA ggplot theme
  ea_theme <- theme( plot.background    = element_rect(fill = "lightgrey"),
                     panel.grid.major.x = element_line(color = "gray90"), 
                     panel.grid.minor   = element_blank(),
                     panel.background   = element_rect(fill = "white", colour = "black") , 
                     panel.grid.major.y = element_line(color = "gray90"),
                     text               = element_text(size = 20),
                     plot.title         = element_text(vjust = 0, colour = "#0B6357  ", face = "bold", size = 30))


  
  