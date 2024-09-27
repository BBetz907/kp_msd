# AUTHOR:   B. Betz | USAID
# PURPOSE:  obtain, explore, and export data on KP-focused community models in Malawi and Cote d'Ivoire
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
  #create active session
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
  # site_urls_for_dsd <- pano_items(site_items) |> filter(str_detect(item, "Congo|Malawi|Cote")) |> pull(path) |> print()
  # 
  # #download
  # map(site_urls_for_dsd, ~grabr::pano_download(item_url = .x, session = sess, dest = path))
  # 
  # IMPORT ------------------------------------------------------------------
  
  file_mli <- glamr::return_latest(folderpath =  path, pattern = "Malawi")
  file_cdi <- glamr::return_latest(folderpath =  path, pattern = "Cote")
  file_drc <- glamr::return_latest(folderpath =  path, pattern = "Congo")
  
  files <- c(file_mli, file_cdi, file_drc)
  
  data_source <- str_extract(file_mli, ("MER.+(?=_[A-Z][a-z]+\\.zip$)")) %>% print()
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
mwi_dic |> count(snu1, snu2, cop22_psnu, psnu, community, communityuid, facility)

#generate list of facilityuid with DICs
mwi_dic_facilityuid <- mwi_dic  |> 
            group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

#generate list of communityuid with DICs
mwi_dic_communityuid <- mwi_dic  |> 
  group_by(communityuid) |> summarise(.groups = "drop") |> pull()

#generate list of psnuuid with DICs
mwi_dic_psnuuid <- mwi_dic  |> 
  group_by(psnuuid) |> summarise(.groups = "drop") |> pull()

#generate list of KP mechanisms
mwi_kp_mech <- df |> filter(country=="Malawi", 
                            mech_code %in% c("81759", "81764", "70190"),
                            otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()

#generate list of districts with KP mechanisms
mwi_kp_mech_psnu <- df |> filter(country=="Malawi", 
                            mech_code %in% c("81759", "81764", "70190"),
                            otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(psnu) |> summarise(.groups = "drop") |> pull()


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

#generate list of districts with KP mechanisms
cdi_kp_mech_psnu <- df |> filter(
  mech_code %in% c("81612", "81611", "84189", "81613"),
  otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID")) |> 
  group_by(psnu) |> summarise(.groups = "drop") |> pull()


## DRC Community ORGUNITs with DICs/Podis--------------  

drc_dic <- df |> filter(
      funding_agency == "USAID",
      mech_code %in% c("84206", "84207", "85505", "85506"),
      otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID"),
      #no filters yet to identify which health zones have DICs
      )

#generate list of  programming areas
drc_dic |> count(snu1, snu2, cop22_psnu, psnu, community, facility)

#generate list of KP mechanisms
drc_kp_mech <- drc_dic |> 
  group_by(mech_code) |> summarise(.groups = "drop") |> pull()

#generate list of districts with KP mechanisms, to share with mission to ID PODIs and DIC/SDPS
drc_kp_mech_psnu <- drc_dic |> 
  group_by(psnu) |> summarise(.groups = "drop") |> pull()

drc_fac <- drc_dic |> group_by(prime_partner_name, mech_code, snu1, snu1uid, community, communityuid, facility, facilityuid) |> 
  summarise(n = n_distinct(indicator), .groups = "drop") |> select(-n)

drc_comm <- drc_fac |> count(prime_partner_name, mech_code, snu1, snu1uid, community, communityuid) |> select(-n)

#output list 
write_csv(drc_comm, "dataout/drc_health_zone_list_community.csv")

write_csv(drc_fac, "dataout/drc_health_zone_list_facility.csv")


## read completed inputs from USAID DRC to ID KP DIC/SDPs and GP PODI AT HEALTH ZONE level
# snu1 is province, community is health zone
file_drc <- glamr::return_latest(folderpath =  path, pattern = "health_zone_list") %>% print()


drc_org_pre <- read_xlsx(file_drc) %>% janitor::clean_names() %>% filter(nchar(mech_code) > 1) 
drc_type <- read_xlsx(file_drc, sheet = 2) %>% janitor::clean_names() %>% glimpse()

drc_org <- drc_org_pre %>% left_join(drc_type) 
#create for filters
drc_org_kp_comm <- drc_org  %>% 
  filter(number_of_kp_dic_sdp > 0)

#create list for detailed information
drc_full_documentation <- drc_org %>%  filter(number_of_kp_dic_sdp > 0 | number_of_gp_podi >0) %>% 
  group_by(mech_code, communityuid, type) %>% 
  summarise(across(starts_with("number_of"), ~ sum(., na.rm = TRUE))) %>% ungroup()

#generate list of facilityuid  - NA because it is all at community_level
# drc_dic_facilityuid <- drc_dic |>
#   group_by(facilityuid) |> summarise(.groups = "drop") |> pull()

#generate list of communityuid
drc_dic_communityuid <- drc_org_kp_comm |> 
  group_by(communityuid) |> summarise(.groups = "drop") |> pull()

#generate list of psnuuid
drc_dic_psnuuid <- drc_dic |> filter(communityuid %in% drc_dic_communityuid)   %>% 
  group_by(psnuuid) |> summarise(.groups = "drop") |> pull()


# create combined lists to apply below
dss_facilityuid <- map(mget(ls(pattern = "_dic_facilityuid$")), ~ .x) %>% reduce(c)
dsd_communityuid <- map(mget(ls(pattern = "_dic_communityuid$")), ~ .x) %>% reduce(c)
dsd_psnuuid <- map(mget(ls(pattern = "_dic_psnuuid$")), ~ .x) %>% reduce(c)
kp_mech <- map(mget(ls(pattern = "_kp_mech$")), ~ .x) %>% reduce(c)
kp_mech_psnu <- map(mget(ls(pattern = "_kp_mech_psnu$")), ~ .x) %>% reduce(c)

## Apply lists above to df to create analytic frame

af <- df |> filter(
             otherdisaggregate %in% c("FSW", "MSM", "TG", "PWID") | (str_detect(standardizeddisaggregate, "Total") & indicator != "KP_PREV"),
             # cumulative > 0
             ) |> 
  mutate(
    #identify site org units which are DICs from lists generated above
    site_dic = case_when(facilityuid %in% dss_facilityuid ~ "DIC",
                          facilityuid == "~" ~ "above site",
                          .default = "not a DIC"),
    #identify community org units with DICs from lists generated above
    community_dic = case_when(communityuid %in% dsd_communityuid ~ "DIC",
                          .default = "no DICs"),
    #identify psnu org units with DICs from lists generated above
    psnu_dic = case_when(psnuuid %in% dsd_psnuuid ~ "DIC",
                          .default = "no DICs"),
    #Identify KP mechanisms from lists above
    mech_type = case_when(mech_code %in% kp_mech ~ "KP",
                               .default = "other"),
    #Identify psnus with KP mechanisms from lists above
    psnu_mech_type = case_when(psnu %in% kp_mech_psnu ~ "KP mech in district",
                          .default = "other"),
    
         ) |> 
    # group_by(across(-c("targets":"cumulative"))) |> 
    # summarize(across(c("targets":"cumulative"), ~sum(., na.rm = TRUE)), .groups = "drop") |> 
  reshape_msd() |>
    group_by(across(-c("value"))) |>
    summarize(across(c("value"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
  
  mutate(indicator = case_when(indicator %in% c("TX_PVLS", "TX_TB") ~ str_c(indicator, numeratordenom, sep = "_"),
                               .default = indicator
                               ),
         disagg = case_when(str_detect(standardizeddisaggregate, "Total") ~ "Total",
                            str_detect(standardizeddisaggregate, "KeyPop") ~ "KP"),
         #map this later to do for all orgunit leels
         comparison_community = case_when(
           community_dic == "DIC" ~ "DIC; KP mechanism",
           .default = if_else(psnu_mech_type == "KP mech in district",
                              # psnu %in% cdi_kp_mech_psnu | 
                                # psnu %in%  mwi_kp_mech_psnu, 
                              "no DIC; KP mechanism", 
                              "no DIC; no KP mechanism"),),
           data_source = data_source,
         ) |> 
  #establish type as urban or rural or null. DRC has rural and urban input. Others only urban.
  left_join(drc_type, by = c("snu1" = "province", "community" = "health_zone")) %>% 
  mutate(type = if_else(!is.na(type),
                        type,
                        case_when(country == "Cote d'Ivoire" & (str_detect(snu1, "Abidjan") | str_detect(snu2, "Bouake") ) ~ "Urban",
                                  country == "Malawi" & (str_detect(psnu, "Lilongwe|Blantyre") ) ~ "Urban"
                        ))) %>%
  glimpse()

#create max periods info
max_fyy <- af %>% filter(period_type == "cumulative") %>% 
  count(period) %>% 
  mutate(max_fy = max(str_extract(period, "(?<=FY)[0-9]{2}"))) %>% 
  count(max_fy) %>% pull(max_fy)

max_dates <- af %>% filter(period_type=="results",
                         str_detect(period, max_fyy)) %>% 
  count(period) %>% 
  mutate(max_q = max(str_extract(period, "[1-4]$")),
         max_fy = str_extract(period, "FY[0-9]{2}"),
         max_fyq = str_c(max_fy, max_q, sep = "Q"),
         max_qq = str_c("Q", max_q)) %>% 
  select(contains("max")) %>% 
  group_by_all() %>% summarise() %>%  
  mutate(join=1) %>% 
  print() 

  
# # assess orgunit structure by country ----
af |> filter(country == "Malawi") |>  group_by(country, snu1, snu2, psnu, cop22_psnu, community) |> summarise()
af |> filter(country != "Malawi", community != "Data reported above Community Level" ) |>
  group_by(snu1, snu2, cop22_psnu, psnu, community, facility) |> summarise() |> print(n=1200)
# 
# 
# #assess dic levels
# af |> filter(psnu_dic == "DIC") |> 
#   group_by(country, site_dic) |> 
#   summarise(n = n_distinct(site_dic, facility))
# 
# af |> filter(community_dic == "DIC") |> 
#   group_by(country, site_dic) |> 
#   summarise(n = n_distinct(site_dic, facility))
# 
# af |> filter(community_dic == "DIC") |> 
#   group_by(country, site_dic) |> 
#   summarise(n = n_distinct(sny, facility))

#add max info
wf <- af %>% mutate(join=1) %>% inner_join(max_dates)

# output -------
write_csv(wf, "Dataout/dsd_cdi_mwi.csv")

