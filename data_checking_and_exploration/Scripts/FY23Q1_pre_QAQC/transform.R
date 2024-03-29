library(tidyverse)
library(janitor)
library(gagglr)
library(scales)


table(mer_df$indicator)

indicator_list <- c("KP_PREV",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "PrEP_CURR", "HTS_TST_POS",
                    "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW")

df <- mer_df %>%  filter(
                    str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE) %>%
                  mutate(indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_",numeratordenom)),
                         funding_agency = recode(funding_agency, "HHS/CDC" = "CDC")) %>%
                  filter(indicator %in% indicator_list) %>% mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% glimpse()


check <- df %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(
        cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         tx_ml_reason = case_when(indicator=="TX_ML" ~ str_extract(otherdisaggregate, "(?<=Outcome\\s-\\s).+")),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, tx_ml_reason, keypop, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) 

qcheck <- df %>% filter(fiscal_year >= 2022, #cumulative and targets
                       str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
                       disaggregate != "KeyPop/Status") %>%
  mutate(fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, keypop, fy, qtr1, qtr2, qtr3, qtr4) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
  mutate(results = coalesce(results, 0),
         qtr = str_replace(qtr, "qtr","Q"),
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr))

rm(df)

modality <- mer_df %>% filter(str_detect(standardizeddisaggregate, "KeyPop|Total") == FALSE,
                          str_detect(indicator, "HTS_TST") == TRUE) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
  mutate(results = coalesce(results, 0),
         qtr = str_replace(qtr, "qtr","Q"),
         fy = fiscal_year,
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr),
         age = trendscoarse) %>%
  select(operatingunit, country, snu1, psnu, prime_partner_name, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, modality, fy, fyq, results, age, ageasentered, sex) %>%
  glimpse()

mmd <- mer_df %>% filter(str_detect(indicator,"TX_CURR(?!_Lag)"),
                     str_detect(disaggregate, "ARVDispense|Total")) %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         partner = prime_partner_name,
         fy = fiscal_year,
         arv = str_extract(otherdisaggregate, "(?<=-\\s).+")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, arv, otherdisaggregate, fy, targets, cumulative) %>%
  glimpse()


rm(mer_df)

table(check$funding_agency)

