# AUTHOR:   B. Betz | USAID
# PURPOSE:  Find site names to ID DICs
# REF ID:   27585128 
# LICENSE:  MIT
# DATE:     2024-01-18
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  
  ref_id <- "27585128"

# IMPORT ------------------------------------------------------------------
  


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

selected_year <- 2023

# define list to filter
epic_ou_data <- read_csv(file = "data/central_mechanism_mech_codes.csv")

# create list for filtering
epic_ims <- epic_ou_data |>
  pull(mech_code) |>
  # paste(collapse = ", ") |>
  print()
class(epic_ims)


# IMPORT ------------------------------------------------------------------

#identify file names and adust paths
ou <- si_path() |> list.files(pattern = "OU", full.names = TRUE) |> print()
# ou <- "data/" |> list.files(pattern = "OU", full.names = TRUE) |> print()


# KP ----------------------------------------------------------------------
# read files
kp_ou_data <- 
  # read_psd(ou) |>  
  read.delim("C:/Users/bbetz/Projects/kp_msd/data_checking_and_exploration/Data/MER_Structured_Datasets_OU_IM_FY21-24_20231215_v2_1.txt",
             col_names = TRUE) |> 
                       mutate(pop = case_when(str_detect(standardizeddisaggregate, "^KeyPop") ~ "KP"),
                              mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |> 
                        filter(pop == "KP",
                               fiscal_year >= selected_year,
                               !indicator %in% c("HTS_RECENT", "HTS_TST_NEG", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", 
                                                 "TX_NET_NEW", "TX_NET_NEW_SHIFT", "TX_RTT") & !numeratordenom == "D"
                              # mech_code %in% epic_ims
                              ) |> 
                      select(-starts_with("qtr"),
                             -starts_with("age"),
                             -contains("for_age"),
                             -contains("_age_"),
                             -sex,
                             -starts_with("otherdisaggregate"),
                             -categoryoptioncomboname,
                             -starts_with("trends"),
                             -contains("modality"),
                             -starts_with("safe"),
                             -starts_with("status"),
                             -ends_with("status"),
                             -source_name,
                             -standardizeddisaggregate,
                             -starts_with("cop22"),
                             -indicatortype
                             ) |> 
                      group_by(across(c(-cumulative, -targets)))|> 
                      summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop")
  

kp_ou_data |> 
  glimpse()

kp_serving_im_list <- kp_ou_data |> 
                          filter(fiscal_year >= selected_year)  |>
                          group_by(country, mech_code, pop) |>   
                          summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
                          filter(cumulative > 0 | targets > 0) |> 
                          # count(country, psnu) |> filter(n>1) |> 
                              print(n=38) |> 
                          
                          pull(mech_code) |> print()


kp_indicator_list <- kp_ou_data |> 
  filter(fiscal_year >= selected_year)  |>
  group_by(indicator) |>   
  summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
  filter(cumulative > 0 | targets > 0) |> 
  # count(country, psnu) |> filter(n>1) |> 
  print(n=38) |> 
  pull(indicator) |> print()


# Total -------------------------------------------------------------------
#filter site_level_data to get totals for all sites with KP targets or results
total_ou_data <- 
  read_psd(ou) |>
  # read.delim("C:/Users/bbetz/Projects/kp_msd/data_checking_and_exploration/Data/MER_Structured_Datasets_OU_IM_FY21-24_20231215_v2_1.txt") |> 
                          mutate(pop = case_when(str_detect(standardizeddisaggregate, "^Total") ~ "Total"),
                                 mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |> 
                          filter(pop == "Total",
                                 fiscal_year >= selected_year,
                                 mech_code %in% kp_serving_im_list,
                                 indicator %in% kp_indicator_list & 
                                 !numeratordenom == "D"
                          ) |> 
                      select(-starts_with("qtr"),
                             -starts_with("age"),
                             -contains("for_age"),
                             -contains("_age_"),
                             -sex,
                             -starts_with("otherdisaggregate"),
                             -categoryoptioncomboname,
                             -starts_with("trends"),
                             -contains("modality"),
                             -starts_with("safe"),
                             -starts_with("status"),
                             -ends_with("status"),
                             -source_name,
                             -standardizeddisaggregate,
                             -starts_with("cop22"),
                             -indicatortype,
                      ) |> 
                      group_by(across(c(-cumulative, -targets)))|> 
                      summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") 

total_ou_data  |>  glimpse()


# bind the two data sets and ID kp-focused IMs-------------------------------------------------------------------------
#######   does mechanism do KP_PREV? kp testing? PrEP? TX_LINKAGE?

  

mech_data <-  kp_ou_data |> rbind(total_ou_data) |> 
  # mutate(row = row_number()) %>%
  # pivot_wider(names_from = c(pop, fiscal_year), values_from = c(targets, cumulative), 
  #             names_sep = "_",
  #             # names_glue = "{pop}_{.value}"
  #             ) |> 
  janitor::clean_names() |> 
  # group_by_all() |> 
  # summarize(across(starts_with("cumulative") | starts_with("targets"), sum, .names = "{.col}"),  .groups= "drop") |> 
  pivot_longer(names_to = "results_or_targets", values_to = "values", cols = c("cumulative", "targets")) |> 
  group_by(across(c(-values))) |> 
  # summarize(across(starts_with("cumulative") | starts_with("targets"), sum, .names = "{.col}"),  .groups= "drop") |> 
  summarise(values = sum(values), .groups = "drop") 

glimpse(mech_data)


mech_data_wide <- mech_data |> 
  
  #remove GP and combine years
  filter(pop == "KP") |> 
  select(-fiscal_year, -results_or_targets) |> 
  group_by(across(-c(values))) |> 
  summarise(values = sum(values), .groups = "drop") |> 
  
  #pivot wider
  pivot_wider(names_from = indicator, values_from = values)  %>% 
  rename(kp_prev = KP_PREV,
         ) |> 
  mutate(focus = case_when(
                  funding_agency == "USAID" ~ case_when(
                    mech_code %in% epic_ims ~ "KP-focused",
                    kp_prev > 0 & !is.na(kp_prev) & 
                      !operatingunit %in% c("Kenya", "Tanzania", "Uganda", "West Africa Region") 
                      ~ "KP-focused",
                    country %in% c("Papua New Guinea", "Vietnam") | operatingunit == "Western Hemisphere Region" | 
                    operatingunit %in% c("Kenya", "Tanzania", "Uganda", "West Africa Region")
                    ~ "integrated"),
                  
                  funding_agency == "HHS/CDC" ~ case_when(
                    kp_prev > 0 & !is.na(kp_prev) ~ case_when(
                       country %in% c("Guatemala", "South Africa", "India", "Tajikistan") |
                       mech_code %in% c(18244, 85853, 160413) ~ "KP-focused",
                      .default = "integrated"
                    ),
                    mech_code %in% c(81874, 87066, 82054, 18528, 84730, 81034, 18538, 81874, 84487, 83023, 85217, 160679) ~ "KP-focused",
                    (kp_prev == 0 | is.na(kp_prev))
                    &
                    operatingunit %in% c("Asia Region", "Cote d`Ivoire", "Kenya", "Lesotho", "Malawi", "Vietnam", "Western Hemisphere")
                      ~ "integrated"
                    #perhaps need to refine further (half KP vs non)
                  )
  ),
            kp_prev_reporting_mer  = case_when(kp_prev > 0  ~ "Reports KP_PREV",
                                           .default = "No KP_PREV"),
            kp_tst_reporting_mer = case_when((HTS_SELF > 0 | HTS_TST > 0) & HTS_TST_POS >  0 ~ "KP testing and case identification", 
                                         (HTS_SELF > 0 | HTS_TST > 0) & HTS_TST_POS <= 0 ~ "KP testing but NOT case identification",
                                         .default = "No KP testing or case identification"),
            kp_tx_linkage_reporting_mer = case_when(HTS_TST_POS >0 & TX_NEW > 0.75*HTS_TST_POS ~ "Mostly proxy linkage", 
                                 HTS_TST_POS >0 & TX_NEW > 0                ~ "Some proxy linkage, referrals for TX or poor linkage",
                                 HTS_TST_POS >0 & (TX_NEW == 0 | is.na(TX_NEW)) ~ "No proxy linkage, likely referrals for TX",
                                 .default = kp_tst_reporting_mer),
            kp_prep_reporting_mer = case_when(PrEP_NEW > 0 | PrEP_CT > 0 ~ "PrEP",
                                   .default =  "No PrEP")
  ) |>  
  rename(KP_PREV = kp_prev,
         indigenous_prime_partner_mer = is_indigenous_prime_partner) |> 
  glimpse()
 
#Create table by of models, focus, reporting, by IM and country Then Join with original.
#Calculate the above for targets and FY separately -- probably preferable --            kp_tst_reporting = case_when(HTS_SELF > 0 | HST_TST > 0) & HTS_TST_POS >0 ~ "KP testing and case identification", 



mech_data_long <- mech_data_wide |> 
  pivot_longer(names_to = "indicator", values_to = "values", cols = "HTS_SELF":"TX_PVLS") |> glimpse()

mech_data |> filter(str_detect(operatingunit, "Region")) |> count(operatingunit)


#mech info
mech_data_wide




# site level data with KP data-------------------------------------------------------------------------

#identify file names and adust paths
full_site_files <- paste0(si_path(), "/site-level") |> list.files(full.names = TRUE) |> print()

#read data and transform
kp_site_reporting <- map_dfr(full_site_files, ~read_psd(.x) |>  
                          mutate(pop = case_when(str_detect(standardizeddisaggregate, "^KeyPop") ~ "KP"),
                                 mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |> 
                          filter(pop == "KP",
                                 fiscal_year >= selected_year,
                                 mech_code %in% kp_serving_im_list,
                                 indicator %in% kp_indicator_list,
                                 # !indicator %in% c("HTS_RECENT", "HTS_TST_NEG", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", 
                                                   # "TX_NET_NEW", "TX_NET_NEW_SHIFT", "TX_RTT") & !numeratordenom == "D"
                          )) |> 
  group_by(country, mech_code, sitetype, indicator) |> 
  summarize(cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") |> 
  filter(cumulative > 0) |> 
  count(country, mech_code, sitetype) |> 
  mutate(n = case_when(n >=1 ~ 1)) |> 
  pivot_wider(names_from = "sitetype", values_from = "n", ) |> janitor::clean_names() |> 
  mutate(facility_level_mer_reporting = case_when(facility == 1 ~ "Yes"),
         community_level_mer_reporting = case_when(community == 1 ~ "Yes"),
         above_site_mer_reporting = case_when(is.na(facility) & is.na(community) ~ "Yes")) |> 
  select(-facility, -community)

# join with ou level data to ID reporting level and fill out all-------------------------------------------------------------------------



#create list of IMs with KP targets with KP targets or results
mech_data_summary  <- mech_data_wide |>
  #use join with site-level data to identify MER reporting level
left_join(kp_site_reporting) |>
  #add back variables
  mutate(
         above_site_mer_reporting = case_when(  is.na(above_site_mer_reporting) & 
                                                is.na(community_level_mer_reporting) &
                                                is.na(facility_level_mer_reporting) ~ "Yes, above site only",
                                                .default = above_site_mer_reporting),
         `poc report: is this an indigenous prime partner` = NA,
         `poc report: mech focus` =  NA,
         `poc report: prep offer or refer` =  NA,
         `poc report: tx linkage approach` =  NA,
         `poc report: does mechanism feature drop-in-centers or one-stop shops` =  NA,
         `poc report: other community model` =  NA,
         `poc report: does mechanism offer differentiated drug delivery?` =  NA,
         `poc report: can MER orgunits identify DICs or OSS?` =  NA
         ) |> 
  count(funding_agency, operatingunit, country, prime_partner_name, mech_type,
        indigenous_prime_partner_mer,  `poc report: is this an indigenous prime partner`, mech_code,  mech_name, focus, `poc report: mech focus`, 
        facility_level_mer_reporting, community_level_mer_reporting, above_site_mer_reporting,
        `poc report: can MER orgunits identify DICs or OSS?`,
        kp_prev_reporting_mer, kp_prep_reporting_mer, `poc report: prep offer or refer`, 
        kp_tst_reporting_mer, kp_tx_linkage_reporting_mer, `poc report: tx linkage approach`, 
        `poc report: does mechanism feature drop-in-centers or one-stop shops` =  NA, `poc report: other community model`, 
        `poc report: does mechanism offer differentiated drug delivery?`) |> 
  select(-n) |> glimpse()

# print using googlesheets4 ------------------------------------------------------

# Find the existing Google Sheets file
# Replace "Existing Google Sheets File Name" with the name of your existing file
      # existing_file <- gs4_find("KP mechanism types and treatment approaches")

sheet_url <- "https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/"

# Get the Google Sheets file using its URL
existing_file <- gs4_get(sheet_url)

# Write a new sheet to the existing file
# Replace "New Sheet Name" with the name you want to give to the new sheet


sheet_write(data = mech_data_summary, ss = existing_file,  sheet = paste0("input_from_r_", as.character(today())))

