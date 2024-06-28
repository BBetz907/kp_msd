# AUTHOR:   B. Betz | USAID
# PURPOSE:  
# REF ID:   0dd282c8 
# LICENSE:  MIT
# DATE:     2024-06-27
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(gagglr)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata
  
  ref_id <- "0dd282c8"

# OU IMPORT ------------------------------------------------------------------
  file <- glamr::return_latest(folderpath =  "Data/", pattern = "OU_IM_FY2")
  metadata <- get_metadata(file) 
  curr_fy <- metadata$curr_fy
  

ou_df <- read_psd(file) |> 
    filter(indicator %in% c("KP_PREV", "PrEP_NEW"),
           standardizeddisaggregate %in% c("Total Numerator", "Total Denominator", "KeyPopAbr", "KeyPop"), 
           fiscal_year >= curr_fy-1) |> 
    reshape_msd() |> 
    filter(period_type!= "results") |> 
    group_by(period, indicator, standardizeddisaggregate, period_type) |> 
    summarise(value=sum(value, na.rm = TRUE), .groups = "drop") |> 
    pivot_wider(names_from = "period_type", values_from = 'value') |> 
    mutate(source = metadata$source) |> 
    arrange(indicator, standardizeddisaggregate)

# IMPORT ------------------------------------------------------------------
file2 <- glamr::return_latest(folderpath =  "Data/", pattern = "PSNU_IM_FY2")
metadata2 <- get_metadata(file2) 
curr_fy2 <- metadata2$curr_fy


psnu_df <-  read_psd(file2) |> 
  filter(indicator %in% c("KP_PREV", "PrEP_NEW"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator", "KeyPopAbr", "KeyPop"), 
         fiscal_year >= curr_fy-1) |> 
  reshape_msd() |> 
  filter(period_type!= "results") |> 
  group_by(period, indicator, standardizeddisaggregate, period_type) |> 
  summarise(value=sum(value, na.rm = TRUE), .groups = "drop") |> 
  pivot_wider(names_from = "period_type", values_from = 'value') |> 
  mutate(source = metadata2$source) |> 
  arrange(indicator, standardizeddisaggregate)

  all.equal(ou_df, psnu_df)
  
  janitor::compare_df_cols(ou_df, psnu_df)	
  
  diffdf::diffdf(ou_df, psnu_df)
  