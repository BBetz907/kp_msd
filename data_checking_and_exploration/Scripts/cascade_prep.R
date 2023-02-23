library(tidyverse)
library(janitor)
library(gagglr)
library(scales)

# Setup local paths where msds live
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- glamr::return_latest(folderpath = merdata, pattern = "PSNU_IM_FY20-23")
file_path

# load msd to be used
df <- gophr::read_msd(file_path, save_rds = FALSE, remove_txt = FALSE)

# populate all the metadata needed for cascade
gamr::get_metadata(file_path)

# Check cascades avaible for creation
plot_name

# Return a cascade data frame (number corresponds to position in char list)
# 1 = Standard cascade
return_cascade(df, 1)

# Plot the cascade
# You will be promted to enter a cascade number
return_cascade_plot(df, export = F)
