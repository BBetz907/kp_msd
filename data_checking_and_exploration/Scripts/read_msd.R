library(tidyverse)
library(glamr)
library(gophr)
getwd()
setwd(si_path())
getwd()

#identify file and file path
MER_Structured_Datasets_PSNU_IM_FY20_23_20220617_v2_1 <- paste0(glamr::si_path(), "/Temp/") %>% return_latest("PSNU_IM_FY2")


# how to open packaged files? ---------------------------------------------


df <- read_msd(file, save_rds = FALSE, remove_txt = FALSE)
# use this function to read data, see guidance
# https://usaid-oha-si.github.io/glamr/reference/read_msd.html
# replace file with path and file from your local computer

