######################################################################
# notes:
# - purpose:
# - inputs:
# - outputs:
# - keywords:
# - general:
######################################################################

#######################################
# load packages and clear objects/log #
#######################################

# load easimple and clear objects log
library(easimple)
ea_start()

# load packages
library(weights)
library(dplyr)
library(reshape2)
library(data.table)
library(eaanalysis)

#############
# set parms #
#############

# output toggle
opt_exp <- 0

#############
# load data #
#############

##########
# export #
##########

# export
if (opt_exp == 1) { 
  
  ea_write( , ".csv")
  
}

