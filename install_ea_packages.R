# install devtools
install.packages("devtools")

# load devtools
library(devtools)

# install easimple (shortcut functions)
devtools::install_github("edanalytics/easimple", auth_token = "39a8d5f688ab0fa803d9954fb698f763731a24c6")

# install eaanalysis (bigger production code)
devtools::install_github("edanalytics/eaanalysis", auth_token = "39a8d5f688ab0fa803d9954fb698f763731a24c6")