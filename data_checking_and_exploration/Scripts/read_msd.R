library(tidyverse)
library(glamr)
library(gophr)


getwd()

#identify file and file path
file <- paste0("Data/") %>% return_latest("PSNU_IM_FY2")

# how to open packaged files? ---------------------------------------------


df <- read_msd(file, save_rds = FALSE, remove_txt = FALSE)
# use this function to read data, see guidance
# https://usaid-oha-si.github.io/glamr/reference/read_msd.html
# replace file with path and file from your local computer

