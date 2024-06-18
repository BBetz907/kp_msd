# AUTHOR:   B. Betz | USAID
# PURPOSE:  
# REF ID:   973616b9 
# LICENSE:  MIT
# DATE:     2024-06-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(stringr)
  library(janitor)
  library(extrafont)
  library(scales)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(glue)
  library(readxl)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata

  ref_id <- "973616b9"

# IMPORT ------------------------------------------------------------------
  
  path <- "Data/site-level/"
  file_mli <- glamr::return_latest(folderpath =  "Data/site-level/", pattern = "Malawi")
  file_cdi <- glamr::return_latest(folderpath =  "Data/site-level/", pattern = "Cote")
  
  files <- c(file_mli, file_cdi)
  
  # filenames <- map(files, ~str_c(path, .x))
  
  df <- map_dfr(files, ~read_psd(file = .x))

# MUNGE -------------------------------------------------------------------

## Malawi: DIC sites and community--------------  
#list of prime partners of KP mechanisms

mwi_dic <- df |> filter(country=="Malawi", 
             mech_code %in% c("81759", "81764", "70190"),
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  filter(!str_detect(facility, "Health|Hospital|Dispensary|Maternity|Clinic|Project|Dream|District|Facility")) 
  
mwi_dic_facilityuid <- mwi_dic  |> 
            group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

mwi_dic_communityuid <- mwi_dic  |> 
  group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

mwi_kp_mech <- df |> filter(country=="Malawi", 
                            mech_code %in% c("81759", "81764", "70190"),
                            otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()


## Cote d'Ivoire DIC sites and community--------------  

cdi_dic <- df |> filter(
  mech_code %in% c("81612", "81611", "84189", "81613"),
  otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  filter(str_detect(facility, "^DIC")) 

cdi_dic_facilityuid <- cdi_dic |> 
  group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

cdi_dic_communityuid <- cdi_dic |> 
  group_by(communityuid) |> summarise(.groups = "drop") |> pull()


cdi_kp_mech <- df |> filter(
    mech_code %in% c("81612", "81611", "84189", "81613"),
    otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()

## Apply

af <- df |> filter(
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID"),
             cumulative > 0) |> 
  mutate(site = case_when(facilityuid %in% cdi_dic_facilityuid | facilityuid %in% mwi_dic_facilityuid ~ "DIC",
                          facilityuid == "~" ~ "above site",
                          .default = "facility"),
         community = case_when(communityuid %in% cdi_dic_communityuid | communityuid %in% mwi_dic_communityuid ~ "DIC",
                          .default = "other"),
         mech_type = case_when(mech_code %in% cdi_kp_mech ~ "KP",
                               mech_code %in% mwi_kp_mech ~ "KP",
                               .default = "other")
         ) |> 
  group_by(across(-c("targets":"cumulative"))) |> 
  summarize(across(c("targets":"cumulative"), ~sum(., na.rm = TRUE)), .groups = "drop") |> 
  reshape_msd() |>
  glimpse()

af |> count(indicator)

 
  # select(-contains("qtr")) |> 
  # group_by(across(-c("cumulative", "targets")))|> 
  # summarize(across(c("cumulative","targets"), ~sum(., na.rm = TRUE)), .groups= "drop") |> 
  # glimpse()


