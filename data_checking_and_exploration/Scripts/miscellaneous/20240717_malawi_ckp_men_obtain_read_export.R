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
  # site_urls_for_dsd <- pano_items(site_items) |> filter(str_detect(item, "Malawi")) |> pull(path) |> print()
  # 
  # #download
  # map(site_urls_for_dsd, ~grabr::pano_download(item_url = .x, session = sess, dest = path))
  
  # IMPORT ------------------------------------------------------------------
  
  file_mli <- glamr::return_latest(folderpath =  path, pattern = "Malawi")
 
  dfmwi <- read_psd(file = file_mli) 

  
  
# MUNGE -------------------------------------------------------------------

# ## Children of KP from both partners--------------  
# ckp_all <- dfmwi |> filter(
#             #restrict to EpiC and Pakachere
#              mech_code %in% c("81759", "70190"),
#              fiscal_year>=2023,
#              ageasentered %in% ckp_ages
#              ) |> 
#   mutate(pop_presumed = "Children of KP")
# 
#   
# ckp_all |> count(mech_code, indicator) |> print(n=32)

## all from Pakachere, which focuses on FSW, and only from FSW focused districts where MACRO implements EpiC--------------  
macro_districts <- c("Machinga District", "Zomba District")
ckp_ages <- c("01-04", "05-09", "10-14", "<01", "<15")


af <- dfmwi |> 
  filter(mech_code == "70190" | 
           (mech_code == "81759" & cop22_psnu %in% macro_districts),
         fiscal_year >= 2023,
         !str_detect(standardizeddisaggregate, "Total|KeyPop"),
         !str_detect(standardizeddisaggregate, "ARV")) |> 
  mutate(
        age_group = case_when(!ageasentered %in% ckp_ages ~ "Adult",
                              ageasentered %in% ckp_ages ~ "Child"),
        pop_presumed = case_when(sex == "Male" &
                          !ageasentered %in% ckp_ages ~ "Clients of sex workers",
                          age_group == "Adult" ~ str_c(age_group, sex, sep = " "),
                          age_group == "Child" ~ "Children",
                          # .default = 
                          )) |> 
  reshape_msd() |>
    group_by(across(-c("value"))) |>
    summarize(across(c("value"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
  mutate(indicator = case_when(indicator %in% c("TX_PVLS", "TX_TB") ~ str_c(indicator, numeratordenom, sep = "_"),
                               
                               str_detect(indicator, "HTS_INDEX_") &  
                                 str_extract(standardizeddisaggregate, "(?<=Sex/).+$") == "Result" ~ 
                                    str_c("HTS_INDEX", "result",
                                          str_extract(indicator, "(?<=X\\_).+$"),
                                          sep = "_"),

                               indicator == "HTS_INDEX" & 
                                 str_extract(standardizeddisaggregate, "(?<=Sex/).+$") != "Result" ~ 
                                    str_c(indicator,
                                          str_extract(standardizeddisaggregate, "^\\d"),
                                          str_extract(standardizeddisaggregate, "(?<=Sex/Index).+$|(?<=Sex/).+$"),
                                          sep = "_"),
                               
                               indicator == "HTS_INDEX" ~ str_c(indicator, 
                                                                str_extract(standardizeddisaggregate, "^\\d"), 
                                                                str_extract(standardizeddisaggregate, "(?<=Sex/).+$"),
                                                                sep = "_"),
                               
                               # indicator == "HTS_INDEX_3_Contacts" ~ str_c(indicator, "Elicited"),
                               # indicator == "HTS_INDEX_4_Results" ~ "HTS_INDEX_4_ContactsTestResults",
                               
                                                              
                               .default = indicator
                               ),
         modality = case_when(str_detect(indicator, "INDEX") | is.na(modality) ~ ".",
                              .default = modality)
         ) |>
  filter(period_type == "cumulative") |> 
  glimpse()

# af |> filter(
#   str_detect(indicator, "INDEX")) |> 
#   count(indicator, standardizeddisaggregate, otherdisaggregate) |> 
#   gt::gt()
# 
# af |> filter(
#   str_detect(indicator, "INDEX")) |> 
#   count(indicator) |> 
#   gt::gt()

write_csv(af, "Dataout/mwi_ckp_clients.csv")

