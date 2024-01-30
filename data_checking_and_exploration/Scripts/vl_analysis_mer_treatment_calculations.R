# AUTHOR:   B. Betz | USAID
# PURPOSE:  load data to run VL analysis
# REF ID:   c65f5050 
# LICENSE:  MIT
# DATE:     2023-09-27
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
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
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
    get_metadata(filepath) 
  
  ref_id <- "c65f5050"
  
  #define list to filter
  site_shift_indicators <- c("TX_CURR_Lag1", "TX_CURR_Lag2", "TX_PVLS")
  excluded_age_disaggs <- c("01-04", "05-09", "10-14",  "<01",  "<15",  "Unknown Age")

# IMPORT ------------------------------------------------------------------

  #identify file names and adust paths
  full_site_files <- paste0(si_path(), "/site-level calculations") |> list.files(full.names = TRUE) |> print()
  #rename, by frst replacing old name formats
  short_site_file_names <- map(full_site_files, ~ str_replace(.x, "MER_Structured_Datasets_SITE_IM_SHIFT_FY[0-9]{2}\\-[0-9]{2}_[0-9]{8}_Site_IM_Shift_", "")) |> print()
  #rename files using walk2
  walk2(full_site_files, short_site_file_names, ~ file.rename(.x, .y))
  
  #re-list (now) shortened file names
  short_names <- paste0(si_path(), "/site-level calculations") |> list.files(full.names = TRUE) |> print()

  
  
  # read and combine files then MUNGE ----------------------------------
  # filter combined files to only include relevant data, summarized to OU level
  
  calc_sites_msd <- map_dfr(short_names, ~read_psd(.x) |>     
                              filter(indicator %in% site_shift_indicators,
                                     standardizeddisaggregate != "Age/Sex/ARVDispense/HIVStatus",
                                     standardizeddisaggregate != "PregnantOrBreastfeeding/Indication/HIVStatus",
                                     !ageasentered %in% excluded_age_disaggs
                                     ) |> 
                              mutate(indicator = case_when(indicator == "TX_PVLS" ~ str_c(indicator, numeratordenom, sep = "_"),
                                                           .default = indicator),
                                     age = case_when(sex %in% c("Male", "Female") ~ "15+"),
                                     pop = case_when(str_detect(standardizeddisaggregate, "KeyPop") ~ otherdisaggregate_sub,
                                                     standardizeddisaggregate %in% c("Total Numerator", "Total Denominator") ~ "Total",
                                                     .default = str_c(sex, age, sep = " ")),
                                     results_value=round(as.double(results_value)),
                                     type = case_when(fiscal_quarter == "Annual" ~ fiscal_quarter,
                                                      .default = "Quarterly"),
                                     fyq = case_when(fiscal_quarter!="Annual" ~ str_c("FY", as.character(fiscal_year-2000), fiscal_quarter))) |> 
                              group_by(operatingunit, country, funding_agency, sitetype, indicator, numeratordenom, 
                                       pop, sex, age, 
                                       # standardizeddisaggregate,
                                       fiscal_year, fiscal_quarter, fyq, type,
                                       mech_code, mech_name, prime_partner_name, prime_partner_duns, is_indigenous_prime_partner,
                                       tx_curr_lag1_mech_change_type, tx_curr_lag2_mech_change_type ) |> 
                              summarise(results = sum(results_value),
                                        targets = sum(targets_value),
                                        .groups = "drop") 
                            ) 
  

  # write_csv(calc_sites_msd, "Dataout/calculations_msd.csv")
  calc_sites_msd |> glimpse()
  
  keypop <- c("MSM", "FSW", "TG", "PWID", "People in prisons and other enclosed settings")
  counts <- c("results", "targets")
  newpop <- c("Total no KP", "Male 15+ no MSM", "Female 15+ no FSW")
  
  calc_sites_msd_expanded <- calc_sites_msd |> select(-sex, -age) |> 
    # map( .x = counts, ~mutate(.x = if_else(pop %in% keypop, -.x, .x)))
    # mutate(results = if_else(pop %in% keypop, -results, results),
    #        targets = if_else(pop %in% keypop, -targets, targets),) |> 
    pivot_wider(names_from = pop, values_from = c(results, targets)) |> 
    # janitor::clean_names() |> 
    mutate(across(c(`results_Female 15+`:`targets_People in prisons and other enclosed settings`), ~ replace_na(., 0)),
           `results_KP`                =  results_FSW + results_MSM + results_TG + results_PWID + `results_People in prisons and other enclosed settings`, 
           `results_Female 15+ no FSW` = `results_Female 15+`  - results_FSW,
           `results_Male 15+ no MSM`   = `results_Male 15+`    - results_MSM,
           `results_Total no KP`       =  results_Total        - results_FSW - results_MSM - results_TG - results_PWID - `results_People in prisons and other enclosed settings`,
           `targets_KP`                =  targets_FSW + targets_MSM + targets_TG + targets_PWID + `targets_People in prisons and other enclosed settings`, 
           `targets_Female 15+ no FSW` = `targets_Female 15+`  - targets_FSW,
           `targets_Male 15+ no MSM`   = `targets_Male 15+`    - targets_MSM,
           `targets_Total no KP`       =  targets_Total        - targets_FSW - targets_MSM - targets_TG - targets_PWID - `targets_People in prisons and other enclosed settings`) |> 
    pivot_longer(cols = `results_Female 15+`:`targets_Total no KP`,
                 names_to = c(".value", "pop"),
                 names_pattern = "^(results|targets)_(.*)$",
                 values_to = "value") |> 
    mutate(across(c(`results`,`targets`), ~ if_else(pop %in% newpop & .<0 & funding_agency != "Dedup", 0, .))) |>
    glimpse()
  
  # rm(calc_sites_msd)
  write_csv(calc_sites_msd_expanded, "Dataout/calculations_expanded_msd.csv")
  calc_sites_msd_expanded |> count(funding_agency)
m
