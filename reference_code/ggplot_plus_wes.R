# Notes:

#######################################
# load packages and clear objects/log #
#######################################

# load packages
library(data.table)
library(ggplot2)
library(wesanderson)

# clear objects log
ea_start()


pal <- wes_palette(name = "Zissou", type = "continuous")

# histogram of residuals
ggplot(data = sub_teacher_resids, aes(fe_va_resid)) + 
  geom_histogram(breaks = seq(-3, 3, by = .25), 
                 alpha = .75,
                 aes(fill = ..count..)) +
  scale_fill_gradientn(colours = pal)
