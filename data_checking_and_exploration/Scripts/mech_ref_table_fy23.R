library(tidyverse)
library(lubridate)
library(strex)

# fhi <- c("Family Health International", "FHI Development 360 LLC")

rp <- c("West Africa Region", "Asia Region", "Western Hemisphere Region")


# generate list of FHI mechanisms for FY23, by country --------------------------------
mech_id_ref_datim_fy23 <- read_csv("https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv") %>% 
  filter(str_detect(partner, "^FHI|^Family\\sHealth\\sInt"),
         agency == "USAID",
         startdate < "2023-10-01",
         enddate >= "2022-10-01",
         # !str_detect(mechanism, "(?<=\\s\\-\\s)GHSC-QA|(?<=\\s\\-\\s)Quality\\sAssurance|"),
         str_detect(mechanism, "EpiC|Epic|EAWA")) %>% 
  mutate(country = if_else(ou %in% rp, str_extract(mechanism, "(?<=\\)\\s\\-\\s).+$"), ou),
         mech_name = str_after_nth(mechanism, "\\s\\-\\s", 2)) %>%
  rename(mech_code = code) %>%
  select(-uid, -primeid,-agency, -enddate, -mechanism) %>% print()


# generate list of FHI mechanisms for FY23 that list OU but not country --------------------------------
nocountry <- mech_id_ref_datim_fy23 %>%   
  filter(is.na(country)) %>% select(-country) %>%
  print(n=22)

# to merge with list of mechanisms with no country, prepare list from the MSD --------------------------------
country_from_msd <- check %>% 
  filter(fy %in% c(2022,2023),
         operatingunit %in% rp,
         str_detect(partner, "^FHI|^Family\\sHealth\\sInt"),
         funding_agency == "USAID") %>% 
  select(country, mech_code, fy) %>% 
  group_by_all() %>% summarise() %>%
  arrange(country, mech_code, fy) %>% print(n=22)

country_from_msd_2023 <- country_from_msd %>% filter(fy == 2023) %>% select(-fy)

#generate 2 splits, matched (4bind) and unmatched
rp_2023 <- nocountry %>% left_join(country_from_msd_2023, by = "mech_code")

rp_2023_4bind <- rp_2023 %>%
  filter(!is.na(country)) %>% print()

rp_2023_2fix <- rp_2023 %>%
  filter(is.na(country)) %>% 
  select(-country) %>% print()

#join FY22 summary data with unmatched entries
country_from_msd_2022 <- country_from_msd %>% filter(fy == 2022) %>% select(-fy)

#generate 2 splits, matched (4bind) and unmatched
rp_2022_4bind <- rp_2023_2fix %>% left_join(country_from_msd_2022, by = "mech_code") %>% print()

#bind both
rp_4bind <- bind_rows(rp_2023_4bind, rp_2022_4bind)

# bind table to list all mech names by country ----------------------------

mech_id_ref_table_fy23 <- mech_id_ref_datim_fy23 %>%   
  filter(!is.na(country)) %>% #remove blanks to be added back in next step
  bind_rows(rp_4bind) %>%
  arrange(ou, country, mech_code) %>% 
  select(-startdate)

# adding/cleaning countries & mech codes

mech_id_ref_table_fy23 <- mech_id_ref_table_fy23 %>% 
  mutate(country = str_replace_all(country, fixed("Democratic Republic of the Congo"), "DRC")) %>%
  filter(mech_code != 84522) %>% add_row(mech_code = "83017", partner = "Family Health International", ou = "Asia Region",
                                         country = "Philippines", mech_name = "FHI 360 Epic")
  
#note some countries may have more than 1 mechanism per country. To avoid duplicating when merging by country (to obtain the mech information)
#You will need to generate a list of countries with multiple mechanisms and ascertain from the data or, more likely, from FHI360 (Amy's team)
#which mechanisms are reporting for each country/geographic unit. Then filter out the mechanisms which exist but are note reporting the data. If you do not do this you will multiply the values 
#by the number of merges by country and overreport. 

#please filter below before exporting the final reference table

# export -------------------------------------------------------------------


write_csv(mech_id_ref_table_fy23, "./Dataout/mech_id_ref_table_fy23.csv")

