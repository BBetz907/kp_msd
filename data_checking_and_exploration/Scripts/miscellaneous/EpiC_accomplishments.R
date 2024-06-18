# AUTHOR:   B. Betz | USAID
# PURPOSE:  Document EpiC results
# REF ID:   d5aa33fb 
# LICENSE:  MIT
# DATE:     2024-06-07
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
  library(googlesheets4)

# GLOBAL VARIABLES --------------------------------------------------------
  
  # SI specific paths/functions  
    load_secrets()

  # Grab metadata

  ref_id <- "d5aa33fb"

# IMPORT ------------------------------------------------------------------
  
  file <- glamr::return_latest(folderpath =  si_path(), pattern = "OU")
  meta <- get_metadata(file)
  df <- read_psd(file)
  
  epic_ou_data_old <- read_csv(file = "data/central_mechanism_mech_codes.csv") |> 
    mutate(mech_code = as.character(mech_code))
  
  sheet_id <- as_sheets_id("https://docs.google.com/spreadsheets/d/1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw/edit#gid=1844478986")
  epic_ou_data_new <- googlesheets4::read_sheet(ss = "1G3K5rXeU54FN54FehPa6LCsJLnAx0rD5aVtrJ5ys-hw", sheet = 3) |> 
    mutate(mech_code = as.character(mech_code))
  
# MUNGE -------------------------------------------------------------------
  epic_fycurr <- df %>% kp_setup() |> kp_clean(time = "fy") |> 
    filter(fy == meta$curr_fy,
           cumulative >0 & !is.na(cumulative)) |> 
    semi_join(epic_ou_data_new, by = "mech_code")

  kp_prev_country_count <- epic_fycurr |> 
    filter(indicator=="KP_PREV") |>        
    summarize(count = n_distinct(country)) |> pull() 
  
  kp_prev_cumulative <- epic_fycurr |> 
    filter(indicator=="KP_PREV",
           standardizeddisaggregate == "KeyPop") |>   
    summarize(cumulative = sum(cumulative)) |> pull()  
  
  hts_cumulative <- epic_fycurr |> 
    filter(indicator=="HTS_TST",
           standardizeddisaggregate == "KeyPop/Result") |>   
    summarize(cumulative = sum(cumulative)) |> pull() 
  
  
  prep_new_cumulative <- epic_fycurr |> 
    filter(indicator=="PrEP_NEW",
           standardizeddisaggregate == "KeyPopAbr"
           ) |> 
    summarize(cumulative = sum(cumulative)) |> pull()
  
  tx_curr_cumulative <- epic_fycurr |> 
    filter(indicator=="TX_CURR",
           standardizeddisaggregate == "KeyPop/HIVStatus"
    ) |> 
    summarize(cumulative = sum(cumulative)) |> pull()
  
  glue::glue("Scaled KP programs in more than {kp_prev_country_count} countries,
             reaching {scales::comma(kp_prev_cumulative)} KP with preventive services, and {scales::comma(hts_cumulative)} with testing, 
             initiating {scales::comma(prep_new_cumulative)} KP on PrEP in the first {meta$curr_qtr} quarters of {meta$curr_fy_lab} to contribute to HIV epidemic control.
             Additionally, they supported a TX cohort of {scales::comma(tx_curr_cumulative)} KP.") 
  