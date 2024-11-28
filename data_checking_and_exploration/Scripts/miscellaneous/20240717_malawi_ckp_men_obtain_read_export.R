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
  file_tnz <- glamr::return_latest(folderpath =  path, pattern = "Tanzania")
  files <- c(file_mli, file_tnz)

df2 <- purrr::map_dfr(files, ~read_psd(file = .x))
  
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
kp <- c("MSM", "TG", "FSW")

df <- df2 |>
  filter(mech_code %in%  c("70190", "81759", "81965", "80095", "160646", "160645", "18488", "87004", "160647"),
         fiscal_year >= 2023,
         !str_detect(standardizeddisaggregate, "Total") ,
         !(str_detect(standardizeddisaggregate, "KeyPop") & !otherdisaggregate_sub %in% kp),
         !str_detect(standardizeddisaggregate, "ARV")
         ) |> 
  mutate(mech_partner = case_when(mech_code == "70190" ~ "Pakachere",
                                  mech_code == "81759" & cop22_psnu %in% macro_districts ~ "EpiC/MACRO",
                                  mech_code == "81759" ~ str_c("EpiC/Other", indicatortype, sep="_")),
        age_group = case_when(!ageasentered %in% ckp_ages ~ "Adult",
                              ageasentered %in% ckp_ages ~ "Child"),
        pop_presumed = case_when(sex == "Male" & 
                                 (mech_code == "70190" | cop22_psnu %in% macro_districts) &
                                 !ageasentered %in% ckp_ages ~ "Clients of sex workers",
                          otherdisaggregate_sub %in% kp ~ otherdisaggregate_sub,
                          age_group == "Adult" ~ str_c(age_group, sex, sep = " "),
                          age_group == "Child" ~ "Children",
                          # .default = 
                          )) |> 
  reshape_msd() |>
    group_by(across(-c("value"))) |>
    summarize(across(c("value"), ~sum(., na.rm = TRUE)), .groups = "drop") |>
    mutate(indicator = case_when(indicator %in% c("TX_PVLS", "TX_TB") ~ 
                                   str_c(indicator, numeratordenom, sep = "_"),
                               
                               str_detect(indicator, "HTS_INDEX_") &  
                                 str_extract(standardizeddisaggregate, "(?<=Sex/).+$") == "Result" ~ 
                                    str_c("HTS_INDEX", "Tested",
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

#explore data, reshape
df %>% filter(str_detect(indicator, "^HTS_INDEX")) %>% count(indicator, standardizeddisaggregate)

df_index <- df %>% filter(str_detect(indicator, "^HTS_INDEX")) %>%
  mutate(order = as.numeric(str_extract(standardizeddisaggregate, "^[1-4]")),
         order = if_else(is.na(order), 5, order),
         index_cascade_sex = case_when(order <= 2 ~ 
                                         if_else(sex=="Male", "Female", "Male"),
                                       .default = sex),
         indicator_2 = case_when(order <= 4 ~ str_extract(indicator, "(?<=\\_[1-4]\\_).+$"),
                                .default = indicator),
         indicator_3 = if_else(order==5, str_extract(indicator_2, "HTS_INDEX_Tested"), indicator_2),
         color = if_else(order==5, str_c(str_extract(indicator, "(?<=\\Tested_).+"), sep = " - "), sex)
         
         ) %>%
  # count(order, indicator, indicator_2, indicator_3, sex,index_cascade_sex, standardizeddisaggregate, otherdisaggregate, statushiv) %>%
  print(n=30)

df_other <- df %>% filter(!str_detect(indicator, "^HTS_INDEX")) 


af <- df_index %>% 
  bind_rows(df_other) %>% glimpse()

af |> filter(
  str_detect(indicator, "INDEX")) |>
  count(indicator, standardizeddisaggregate, otherdisaggregate) |>
  gt::gt()

af |> filter(
  str_detect(indicator, "INDEX")) |>
  group_by(indicator) |> summarise(value=sum(value, na.rm = TRUE)) |> 
  gt::gt()

write_csv(af, "Dataout/ckp_clients.csv")

