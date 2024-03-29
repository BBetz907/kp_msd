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
full_site_files <- paste0(si_path(), "/site-level") |> list.files(full.names = TRUE) |> print()

# esw <- read_psd(full_site_files[6]) |> glimpse()
          # mutate(mech_code = as.character(mech_code)) |>
#           filter(mech_code %in% epic_ims)
# 
# esw |> 
#   mutate(mech_code = as.character(mech_code)) |> 
#   filter(mech_code== "84244")

## test
read_psd(full_site_files[3]) |> mutate(pop = case_when(str_detect(standardizeddisaggregate, "^KeyPop") ~ "KP",
                                                       str_detect(standardizeddisaggregate, "^Total") ~ "Total"),
                mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |>
        filter(pop %in% c("KP", "Total"),
                fiscal_year >= selected_year  ) |>
  select(-starts_with("qtr"),
         -starts_with("age"),
         -contains("for_age"),
         -contains("_age_"),
         -sex,
         -starts_with("otherdisaggregate"),
         -categoryoptioncomboname,
         -starts_with("trends"),
         -ends_with("modality"),
         -starts_with("safe"),
         -starts_with("status"),
         -source_name,
         -standardizeddisaggregate,
         -starts_with("cop22"),
         -snuprioritization
         ) |>
  group_by_all() |>
  summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |>
  glimpse()

# KP ----------------------------------------------------------------------
# read files
kp_site_data <- map_dfr(full_site_files, ~read_psd(.x) |>  
                       mutate(pop = case_when(str_detect(standardizeddisaggregate, "^KeyPop") ~ "KP"),
                              mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |> 
                        filter(pop == "KP",
                               fiscal_year >= selected_year
                              # mech_code %in% epic_ims
                              )) |> 
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
                             -snuprioritization
                             ) |> 
                      group_by_all() |> 
                      summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
                      glimpse()
  

kp_site_data |> 
  glimpse()


kp_serving_im_list <- kp_site_data |> 
                          filter(fiscal_year >= selected_year)  |>
                          group_by(country, mech_code, pop) |>   
                          summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
                          filter(cumulative > 0 | targets > 0) |> 
                          # count(country, psnu) |> filter(n>1) |> 
                              print(n=38) |> 
                          
                          pull(mech_code) |> print()


kp_indicator_list <- kp_site_data |> 
  filter(fiscal_year >= selected_year)  |>
  group_by(indicator) |>   
  summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
  filter(cumulative > 0 | targets > 0) |> 
  # count(country, psnu) |> filter(n>1) |> 
  print(n=38) |> 
  pull(indicator) |> print()


# Total -------------------------------------------------------------------
#filter site_level_data to get totals for all sites with KP targets or results
total_site_data <- map_dfr(full_site_files, ~read_psd(.x) |>  
                          mutate(pop = case_when(str_detect(standardizeddisaggregate, "^Total") ~ "Total"),
                                 mech_type = case_when(mech_code %in% epic_ims ~ "Central Mechanism")) |> 
                          filter(pop == "Total",
                                 fiscal_year >= selected_year,
                                 mech_code %in% kp_serving_im_list,
                                 indicator %in% kp_indicator_list
                          )) |> 
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
                             -snuprioritization
                      ) |> 
                      group_by_all() |> 
                      summarize(cumulative = sum(cumulative, na.rm = TRUE), targets = sum(targets, na.rm = TRUE), .groups = "drop") |> 
                      glimpse()

total_site_data  |>  glimpse()


# bind the two data sets and pivot-------------------------------------------------------------------------
site_data <- kp_site_data |> rbind(total_site_data) |> glimpse()
  

mech_data <- site_data |>
  # mutate(row = row_number()) %>%
  # pivot_wider(names_from = c(pop, fiscal_year), values_from = c(targets, cumulative), 
  #             names_sep = "_",
  #             # names_glue = "{pop}_{.value}"
  #             ) |> 
  janitor::clean_names() |> 
  select(
    # -row, 
         -orgunituid, -contains("site"), -contains("facility"), -contains("community"), -contains("snu")) |> 
  # group_by_all() |> 
  # summarize(across(starts_with("cumulative") | starts_with("targets"), sum, .names = "{.col}"),  .groups= "drop") |> 
  pivot_longer(names_to = "results_or_targets", values_to = "values", cols = c("cumulative", "targets")) |> 
  group_by_all() |> 
  # summarize(across(starts_with("cumulative") | starts_with("targets"), sum, .names = "{.col}"),  .groups= "drop") |> 
  summarise(values = sum(values), .groups = "drop") |> 
  glimpse()

#warning message without row_number


#create list of IMs with KP targets with KP targets or results
site_data_simplified  <- site_data |> 
  group_by(fiscal_year, funding_agency, operatingunit, country, psnu, sitetype, sitename,  prime_partner_name, 
           is_indigenous_prime_partner, mech_code, mech_type,  mech_name, indicator) |> 
  summarize(across(starts_with("cumulative_") | starts_with("targets_"), sum, .names = "{.col}"), .groups= "drop") |> glimpse()
            
#bind rows to get totals and KP


# write_csv(site_data_for_tableau, "Dataout/site_data_for_tableau.csv")

# rm(epic_site_data)
write_csv(site_data, "Dataout/kp_sites_full.csv")
# write_csv(kp_site_data_summary, "Dataout/kp_sites.csv")
write_csv(site_data_simplified, "Dataout/kp_sites_simplified.csv")
write_csv(mech_data, "Dataout/kp_mech_simplified.csv")

gc()
