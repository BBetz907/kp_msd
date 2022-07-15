library(tidyverse)
library(janitor)

indicator_list <- c("KP_PREV",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "PrEP_CURR", "HTS_TST_POS",
                    "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW")

df <- df %>% filter(fiscal_year >= 2021, #cumulative and targets
                    fiscal_year < 2023,
                    str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
                    indicator %in% indicator_list)

glimpse(df)
table(df$indicator)

indicator_list <- c("KP_PREV",  "HTS_SELF", "HTS_TST", "HTS_TST_NEG", "PrEP_NEW", "PrEP_CT", "PrEP_CURR", "HTS_TST_POS",
                "TX_NEW", "TX_CURR", "TX_PVLS", "TX_PVLS_D", "TX_PVLS_N", "TX_CURR_Lag1", "TX_CURR_Lag2", "TX_ML", "TX_NET_NEW")

# MER <- df %>% mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>% glimpse()
df

check <- df %>% filter(disaggregate != "KeyPop/Status") %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         tx_ml_reason = case_when(indicator=="TX_ML" ~ str_extract(otherdisaggregate, "(?<=Outcome\\s-\\s).+")),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners"),
         indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_", numeratordenom))) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, tx_ml_reason, keypop, fy, targets, cumulative) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>%
  glimpse()


qcheck <- df %>% filter(fiscal_year >= 2021, #cumulative and targets
                       str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
                       disaggregate != "KeyPop/Status") %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         partner = prime_partner_name,
         disagg = str_extract(standardizeddisaggregate, "Total|KeyPop"),
         disagg = recode(disagg, "KeyPop" = "KP"),
         keypop = str_extract(otherdisaggregate, "FSW|MSM|TG|PWID|People\\sin\\sprisons"),
         keypop = recode(keypop, "People in prisons" = "Prisoners"),
         indicator = recode(indicator, "TX_PVLS" = paste0(indicator,"_", numeratordenom))) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disagg, disaggregate, keypop, fy, qtr1, qtr2, qtr3, qtr4) %>%
  pivot_longer(qtr1:qtr4, names_to = "qtr", values_to = "results" ) %>%
  mutate(qtr = str_replace(qtr, "qtr","Q"),
         fyq = paste0("FY",str_extract(fy, "..$"), " ", qtr)) %>%
  mutate(indicator = factor(indicator, levels = indicator_list)) %>% arrange(indicator) %>%
  glimpse()

qcheck

modality <- df %>% filter(str_detect(standardizeddisaggregate, "KeyPop|Total") == FALSE,
                          str_detect(indicator, "HTS_TST") == TRUE) %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         fy = fiscal_year,
         age = trendscoarse) %>%
  select(operatingunit, country, snu1, psnu, prime_partner_name, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, modality, fy, targets, cumulative, age, ageasentered, sex) %>%
  glimpse()

mmd <- df %>% filter(str_detect(indicator,"TX_CURR(?!_Lag)"),
                     str_detect(disaggregate, "ARVDispense|Total")) %>%
  mutate(cumulative = coalesce(cumulative, 0),
         targets = coalesce(targets, 0),
         partner = prime_partner_name,
         fy = fiscal_year,
         arv = str_extract(otherdisaggregate, "(?<=-\\s).+")) %>%
  select(operatingunit, country, snu1, psnu, partner, mech_code, mech_name, indicator, funding_agency, numeratordenom, disaggregate, arv, otherdisaggregate, fy, targets, cumulative) %>%
  glimpse()
table(mmd$indicator, mmd$disaggregate)

# help("coalesce")
