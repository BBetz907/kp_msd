# AUTHOR:   B. Betz | USAID
# PURPOSE:  
# REF ID:   973616b9 
# LICENSE:  MIT
# DATE:     2024-06-06
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)
  library(grabr)
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
  path <- paste0(si_path(),"/site-level/")
  
  # # OBTAIN DATA ------------------------------------------------------------------
  # #create active session
  # sess <- grabr::pano_session(username = pano_user(), password = pano_pwd())
  # 
  # # Extract data items details ----------------------------------------------
  # url <- "https://pepfar-panorama.org/forms/downloads/"
  # 
  # dir_items <- pano_items(page_url = url, 
  #                         username = pano_user(),
  #                         password = pano_pwd()) 
  # 
  # mer_items_path <-  dir_items |> filter(str_detect(item, "MER\\sFY20[0-9]{2}\\sQ[1-4]")) |> 
  #   pull(path) 
  # 
  # mer_items <- pano_items(mer_items_path)
  # 
  # site_items <- mer_items |> filter(item == "Site Level") |> pull(path)
  # site_urls_for_dsd <- pano_items(site_items) |> filter(str_detect(item, "Malawi|Cote")) |> pull(path) |> print()
  # 
  # #download
  # map(site_urls_for_dsd, ~grabr::pano_download(item_url = .x, session = sess, dest = path))
  
  # IMPORT ------------------------------------------------------------------
  
  file_mli <- glamr::return_latest(folderpath =  path, pattern = "Malawi")
  file_cdi <- glamr::return_latest(folderpath =  path, pattern = "Cote")
  
  files <- c(file_mli, file_cdi)
  
  # filenames <- map(files, ~str_c(path, .x))
  
  df <- map_dfr(files, ~read_psd(file = .x))

# MUNGE -------------------------------------------------------------------

## Malawi: DIC sites and community--------------  
#list of prime partners of KP mechanisms

mwi_dic <- df |> filter(country=="Malawi", 
             mech_code %in% c("81759", "81764", "70190"),
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  filter(!str_detect(facility, "Health|Hospital|Dispensary|Maternity|Clinic|Project|Dream|District|Facility|Assembly")) 
  
#confirmed that this list contains 18 DICs, no other facility types
mwi_dic |> count(snu1, snu2, cop22_psnu, psnu, community, facility)

#generate list facilityuid with DICs
mwi_dic_facilityuid <- mwi_dic  |> 
            group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

#generate list ofcommunityuid with DICs
mwi_dic_communityuid <- mwi_dic  |> 
  group_by(communityuid) |> summarise(.groups = "drop") |> pull()

#generate list ofcommunityuid with DICs
mwi_dic_psnuuid <- mwi_dic  |> 
  group_by(psnuuid) |> summarise(.groups = "drop") |> pull()

#generate list of KP mechanisms
mwi_kp_mech <- df |> filter(country=="Malawi", 
                            mech_code %in% c("81759", "81764", "70190"),
                            otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()


## Cote d'Ivoire DIC sites and community--------------  

cdi_dic <- df |> filter(
  mech_code %in% c("81612", "81611", "84189", "81613"),
  otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  filter(str_detect(facility, "^DIC")) 

#confirmed that this list contains only the 3 known DICs in Cote d'Ivpore
cdi_dic |> count(snu1, snu2, cop22_psnu, psnu, community, facility)

#generate list of facilityuid with DICs
cdi_dic_facilityuid <- cdi_dic |> 
  group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

#generate list ofcommunityuid with DICs
cdi_dic_communityuid <- cdi_dic |> 
  group_by(communityuid) |> summarise(.groups = "drop") |> pull()

#generate list of psnuuid with DICs
cdi_dic_psnuuid <- cdi_dic |> 
  group_by(psnuuid) |> summarise(.groups = "drop") |> pull()

#generate list of KP mechanisms
cdi_kp_mech <- df |> filter(
    mech_code %in% c("81612", "81611", "84189", "81613"),
    otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()


## Apply lists above to df to create analytic frame

af <- df |> filter(
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID"),
             cumulative > 0) |> 
  mutate(
    #identify site org units which are DICs from lists generated above
    site_dic = case_when(facilityuid %in% cdi_dic_facilityuid | facilityuid %in% mwi_dic_facilityuid ~ "DIC",
                          facilityuid == "~" ~ "above site",
                          .default = "facility"),
    #identify community org units with DICs from lists generated above
    community_dic = case_when(communityuid %in% cdi_dic_communityuid | communityuid %in% mwi_dic_communityuid ~ "DIC",
                          .default = "other"),
    #identify psnu org units with DICs from lists generated above
    psnu_dic = case_when(psnuuid %in% cdi_dic_psnuuid | psnuuid %in% mwi_dic_psnuuid ~ "DIC",
                          .default = "other"),
    #Identify KP mechanisms from lists above
    mech_type = case_when(mech_code %in% cdi_kp_mech ~ "KP",
                               mech_code %in% mwi_kp_mech ~ "KP",
                               .default = "other") |> 
         ) |> 
  group_by(across(-c("targets":"cumulative"))) |> 
  summarize(across(c("targets":"cumulative"), ~sum(., na.rm = TRUE)), .groups = "drop") |> 
  reshape_msd() |>
  glimpse()

af |> count(country, mech_type, psnu_dic, community, community_dic, facility, site_dic)

af |> filter(psnu_dic == "DIC") |> 
  group_by(country, site_dic) |> 
  summarise(n = n_distinct(site_dic, facility))

af |> filter(community_dic == "DIC") |> 
  group_by(country, site_dic) |> 
  summarise(n = n_distinct(site_dic, facility))

write_csv(af, "Dataout/dsd_cdi_mwi.csv")
