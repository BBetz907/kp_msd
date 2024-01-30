# AUTHOR:   B. Betz | USAID
# PURPOSE:  
# REF ID:   27585128 
# LICENSE:  MIT
# DATE:     2024-01-18
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  sipack

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "27585128"

# IMPORT ------------------------------------------------------------------
  
  return_data
  msd

# MUNGE -------------------------------------------------------------------

library(glamr)
library(tidyverse)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(readxl)
library(googlesheets4)



#define list to filter
# epic_ou_data <- read_csv(file = "data/central_mechanism_mech_codes.csv")

#create list for filtering
epic_ims <- epic_ou_data |> 
  pull(mech_code) |>
  # paste(collapse = ", ") |>
  print()
class(epic_ims)


# IMPORT ------------------------------------------------------------------

#identify file names and adust paths
full_site_files <- paste0(si_path(), "/site-level") |> list.files(full.names = TRUE) |> print()

# esw <- read_psd(full_site_files[6]) |>     
#           # mutate(mech_code = as.character(mech_code)) |> 
#           filter(mech_code %in% epic_ims)
# 
# esw |> 
#   mutate(mech_code = as.character(mech_code)) |> 
#   filter(mech_code== "84244")

# read files
epic_site_data <- map_dfr(full_site_files, ~read_psd(.x) |>     
                            filter(mech_code %in% epic_ims))

glimpse(epic_site_data)
epic_org_units <- epic_site_data |> count(operatingunit, country, snu1, snu2, cop22_psnu, psnu, community, facility, sitetype, sitename)
rm(epic_site_data)

write_csv(epic_org_units, "Dataout/epic_sites.csv")
gc()